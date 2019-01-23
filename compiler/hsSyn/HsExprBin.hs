module HsExprBin
  ( exprSE2PS
  , declSE2PS
  , exprPS2SE
  , declPS2SE
  ) where

import GhcPrelude
import HsDecls
import HsExpr
import HsExprBin_Conversions
import qualified HsExprBin_ConversionSE2PS as SE2PS
import qualified HsExprBin_ConversionPS2SE as PS2SE
import HsExprBin_Instances ()
import HsExtension
import TcRnTypes

{-

Note [Serialisable AST phase]

There is an AST phase called GhcSe, where 'Se' stands for Serialisable.
It is quite close to GhcPs, in that it mostly represents ASTs the same way,
except for (syntax-level) types and names, which are represented in a way
that is (binary) serialisation friendly.

The motivation for this new phase is to be able to serialise ASTs of Haskell
code. (No existing phase has this property.) One use case would be to save
ASTs resulting from the evaluation of Template Haskell code and to reuse them
later, in place of evaluating the Template Haskell code. More generally,
it seems useful to be able to persist or load parsed ASTs, may it be for
IDE-style interactions or plugins.

The purpose of the HsExprBin* modules is to define:
- conversions from GhcSe to GhcPs, and back, with the four functions exported
  by this module;
- Binary instances for AST data types, only when "instantiated" at the GhcSe
  phase.

The former is done in two modules: HsExprBin_ConversionSE2PS and
HsExprBin_ConversionPS2SE. The latter in HsExprBin_Instances.

-}

-- * High-level conversion interface

-- Converting Se -> Ps

-- | Convert a serialisable expression AST to a parsed expression AST
exprSE2PS :: LHsExpr GhcSe -> RnM (ConvResult (LHsExpr GhcPs))
exprSE2PS = runConv . SE2PS.cvLHsExpr

-- | Convert a serialisable declaration AST to a parsed declaration AST
declSE2PS :: LHsDecl GhcSe -> RnM (ConvResult (LHsDecl GhcPs))
declSE2PS = runConv . SE2PS.cvLHsDecl

-- Converting Ps -> Se

-- | Convert a parsed expression AST to a serialisable expression AST
exprPS2SE :: LHsExpr GhcPs -> RnM (ConvResult (LHsExpr GhcSe))
exprPS2SE = runConv . PS2SE.cvLHsExpr

-- | Convert a parsed declaration AST to a serialisable expression AST
declPS2SE :: LHsDecl GhcPs -> RnM (ConvResult (LHsDecl GhcSe))
declPS2SE = runConv . PS2SE.cvLHsDecl
