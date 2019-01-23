{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SeName (SeName(..), mkSeName) where

import Outputable
import RdrName

newtype SeName = SeName RdrName
  deriving (Outputable, OutputableBndr)

mkSeName :: RdrName -> SeName
mkSeName = SeName
