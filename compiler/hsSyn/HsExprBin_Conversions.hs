{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module HsExprBin_Conversions where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Maybe

import DynFlags
import FastString
import GhcPrelude
import HsExpr
import IfaceType
import Name
import Module
import Outputable
import PackageConfig
import Packages
import RdrName
import SeName
import SrcLoc
import TcRnTypes
import ToIface (toIfaceType)
import TyCoRep (Type(..), TyLit(..))

-- | Conversion error
data ConvError
  = ConvUnsupported String String SDoc -- ^ Unsupported constructor of a type.
                                       --   Constructor name, type name and
                                       --   text rendering of the unsupported
                                       --   subexpression.
  | ConvFailure String                 -- ^ Other errors

-- | Conversion result
data ConvResult a
  = ConvError ConvError
  | ConvOK a
  deriving Functor

-- * Conversion utilities

-- | Conversion monad
newtype Conv a = Conv { runConv :: RnM (ConvResult a) }

instance Functor Conv where
  fmap f (Conv k) = Conv (fmap (fmap f) k)

instance Applicative Conv where
  pure = Conv . return . ConvOK
  (<*>) = ap

instance Monad Conv where
  return = pure

  Conv mx >>= f = Conv $ mx >>= \cvx -> case cvx of
    ConvOK x    -> runConv (f x)
    ConvError e -> pure (ConvError e)

-- | Throw an unsupported construct error in the 'Conv' monad.
unsupported :: String -- ^ constructor name
            -> String -- ^ type name
            -> SDoc   -- ^ textual rendering of the unsupported subexpression
            -> Conv a
unsupported con ty subexpr = Conv $
  pure (ConvError $ ConvUnsupported con ty subexpr)

-- | Throw a bad input error in the 'Conv' monad.
badInput :: String -> Conv a
badInput str = Conv $ pure (ConvError $ ConvFailure str)

-- | Lift a 'RnM' computation to 'Conv'.
liftRn :: RnM a -> Conv a
liftRn = Conv . fmap ConvOK

-- | Convert 'Type' to 'IfaceType'.
typeToIfaceType :: Type -> Conv IfaceType
typeToIfaceType = pure . toIfaceType

-- This function is partial but is only used when dealing with
-- literals, so as long as this holds we won't see the
-- 'unsupported' errors.
-- | (Partially) convert 'IfaceType' to 'Type' (only supports
--   'IfaceLitTy').
ifaceTypeToType :: IfaceType -> Conv Type
ifaceTypeToType (IfaceLitTy n) = pure $ LitTy (go n)
  where go (IfaceNumTyLit a) = NumTyLit a
        go (IfaceStrTyLit a) = StrTyLit a
ifaceTypeToType e@(IfaceFreeTyVar {}) = unsupported "IfaceFreeTyVar" "IfaceType" (ppr e)
ifaceTypeToType e@(IfaceTyVar {}) = unsupported "IfaceTyVar" "IfaceType" (ppr e)
ifaceTypeToType e@(IfaceAppTy {}) = unsupported "IfaceAppTy" "IfaceType" (ppr e)
ifaceTypeToType e@(IfaceFunTy {}) = unsupported "IfaceFunTy" "IfaceType" (ppr e)
ifaceTypeToType e@(IfaceForAllTy {}) = unsupported "IfaceForAllTy" "IfaceType" (ppr e)
ifaceTypeToType e@(IfaceTyConApp {}) = unsupported "IfaceTyConApp" "IfaceType" (ppr e)
ifaceTypeToType e@(IfaceCastTy {}) = unsupported "IfaceCastTy" "IfaceType" (ppr e)
ifaceTypeToType e@(IfaceCoercionTy {}) = unsupported "IfaceCoercion" "IfaceType" (ppr e)
ifaceTypeToType e@(IfaceTupleTy {}) = unsupported "IfaceTupleTy" "IfaceType" (ppr e)


-- | A common name for the functions that convert from and to
--   different name representations.
class ConvertName a b where
  convertName :: a -> Conv b

instance ConvertName a b => ConvertName (Located a) (Located b) where
  convertName = traverse convertName

instance ConvertName a b => ConvertName [a] [b] where
  convertName = traverse convertName

instance ConvertName a b => ConvertName (Either e a) (Either e b) where
  convertName = traverse convertName

instance ConvertName a b => ConvertName (HsMatchContext a) (HsMatchContext b) where
  convertName = traverse convertName

instance ConvertName a b => ConvertName (HsStmtContext a) (HsStmtContext b) where
  convertName = traverse convertName

instance ConvertName a b => ConvertName (Maybe a) (Maybe b) where
  convertName = traverse convertName

instance ConvertName RdrName SeName where
  convertName = pure . mkSeName

instance ConvertName SeName RdrName where
  convertName (SeName n) = case n of
    Orig mod occn -> do
      -- TODO: introduce some caching here, to avoid doing the
      --       searchPackageId dance too often.
      currentMod <- liftRn getModule

      if samePackages currentMod mod
        then let newMod = mod { moduleUnitId = moduleUnitId currentMod } in
               pure (Orig newMod occn)
        else do mnewmod <- liftRn (findEquivalentModule mod)
                case mnewmod of
                  Nothing   -> pure (Orig mod occn)
                  Just mod' -> pure (Orig mod' occn)

    _             -> pure n

    where samePackages mod1 mod2 = fromMaybe False $ do -- maybe monad
            let str1 = unitIdString (moduleUnitId mod1)
                str2 = unitIdString (moduleUnitId mod2)
            (pkg1, ver1, _mhash1) <- parseUnitId' str1
            (pkg2, ver2, _mhash2) <- parseUnitId' str2
            return (pkg1 == pkg2 && ver1 == ver2)

instance ConvertName Name SeName where
  convertName n = pure $ mkSeName (nameRdrName n)

instance ConvertName SeName Name where
  convertName (SeName n) = case isExact_maybe n of
    Just a -> pure a
    _      -> badInput "convertName :: SeName -> Name: non exact RdrName in SeName"

-- * Looking up modules/packages for Orig names

-- this rejects wired in packages, because we want to leave them untouched
parseUnitId' :: String -> Maybe (String, String, Maybe String)
parseUnitId' = parse

  where
    parse s = case splitOn '-' (reverse s) of
      ("":_) -> Nothing
      xs | length xs >= 1 && last xs == "" -> Nothing
      (hash:ver:name) | isVersion ver ->
         Just (intercalate "-" (reverse name), ver, Just hash)
      (ver:name) | isVersion ver ->
         Just (intercalate "-" (reverse name), ver, Nothing)
      _ -> Nothing
    splitOn c = go []
      where go acc (x:xs)
              | x == c    = acc : go "" xs
              | otherwise = go (x:acc) xs
            go acc [] = [acc]
    isVersion = go False
      -- True: waiting for digit or dot (we've seen a digit last)
      -- False: waiting for digit (we've just seen a dot)
      where go False (c:cs)
              | isDigit c = go True cs
              | otherwise = False
            go True (c:cs)
              | isDigit c = go True cs
              | c == '.'  = go False cs
              | otherwise = False
            go b [] = b -- if we've seen a dot last (False), we fail
                        -- otherwise, the version number can end here

-- | Look up the module from the same package, but built by the
--   current compiler, therefore with a slightly different hash
--   in the unit id than the input Module, which was built by some
--   non-cross-compiling GHC.
findEquivalentModule :: Module -> RnM (Maybe Module)
findEquivalentModule mod = do
  liftIO $ putStrLn ("Looking for equivalent to: " ++ unitIdStr)
  case parseUnitId' unitIdStr of
    Nothing -> return Nothing
    Just (pkg, ver, _mhash) -> do
      muid <- lookFor pkg ver
      maybe (pure Nothing) (\uid -> return $ Just (mod { moduleUnitId = uid })) muid

  where unitIdStr = unitIdString (moduleUnitId mod)

lookFor :: String -> String -> RnM (Maybe UnitId)
lookFor pkg ver = do
  dflags <- getDynFlags
  let pkgid = mkFastString (pkg ++ "-" ++ ver)
      pkgs = searchPackageId dflags (SourcePackageId pkgid)
  liftIO $ putStrLn ("Looking for: " ++ pkg ++ "-" ++ ver)
  liftIO . putStrLn . unwords $
    [ "Found", show (length pkgs), "pkgs:" ] ++
    [ unitIdString (packageConfigId p) | p <- pkgs ]
  if null pkgs then pure Nothing else pure (Just $ packageConfigId (head pkgs))
