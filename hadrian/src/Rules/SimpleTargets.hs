module Rules.SimpleTargets (simplePackageTargets) where

import Base
import Context
import Packages
import Settings

import Data.Foldable

-- | Simple aliases for library and executable targets.
--
--  - @stage<N>:lib:<name>@ will build library @name@ with
--    the stage N compiler, putting the result under
--    @<build root>/stage<N>/lib@.
--  - @stage<N>:exe:<name>@ will build executable @name@
--    with the stage N-1 compiler, putting the result under
--    @<build root>/stage<N-1>/bin.
simplePackageTargets :: Rules ()
simplePackageTargets = traverse_ simpleTarget targets

  where targets = [ (stage, target)
                  | stage <- [minBound..maxBound]
                  , target <- knownPackages
                  ]

simpleTarget :: (Stage, Package) -> Rules ()
simpleTarget (stage, target) = do
  let tgt = intercalate ":" [stagestr, typ, pkgname]
  tgt ~> do
    p <- getTargetPath
    need [ p ]

  where typ = if isLibrary target then "lib" else "exe"
        stagestr = stageString stage
        pkgname = pkgName target
        getTargetPath = if isLibrary target
          then pkgConfFile (vanillaContext stage target)
          else programPath (vanillaContext (pred stage) target)
