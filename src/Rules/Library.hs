module Rules.Library (buildPackageLibrary) where

import Way
import Base
import Util
import Builder
import Package
import Switches
import Expression
import qualified Target
import Oracles.PackageData
import Settings.Util
import Settings.TargetDirectory
import Rules.Actions
import Rules.Resources
import Data.List
import qualified System.Directory as IO

buildPackageLibrary :: Resources -> StagePackageTarget -> Rules ()
buildPackageLibrary _ target = do
    let stage     = Target.stage target
        pkg       = Target.package target
        path      = targetPath stage pkg
        buildPath = path -/- "build"

    -- TODO: handle dynamic libraries
    matchBuildResult buildPath "a" ?> \a -> do
        liftIO $ removeFiles "." [a]
        cSrcs   <- interpret target $ getPkgDataList CSrcs
        modules <- interpret target $ getPkgDataList Modules

        let way   = detectWay a
            hSrcs = map (replaceEq '.' '/') modules
            cObjs = [ buildPath -/- src -<.> osuf way | src <- cSrcs ]
            hObjs = [ buildPath -/- src  <.> osuf way | src <- hSrcs ]

        -- This will create split objects if required (we don't track them)
        need $ cObjs ++ hObjs

        split <- interpret target splitObjects
        splitObjs <- if split
            then fmap concat $ forM hSrcs $ \src -> do
                let splitPath = buildPath -/- src ++ "_" ++ osuf way ++ "_split"
                contents <- liftIO $ IO.getDirectoryContents splitPath
                return . map (splitPath -/-)
                       . filter (not . all (== '.')) $ contents
            else return []

        build $ fullTarget target Ar (cObjs ++ hObjs ++ splitObjs) [a]

        synopsis <- interpret target $ getPkgData Synopsis
        putSuccess $ "/--------\n| Successfully built package library '"
            ++ pkgName pkg
            ++ "' (stage " ++ show stage ++ ", way "++ show way ++ ")."
        putSuccess $ "| Package synopsis: "
            ++ dropWhileEnd isPunctuation synopsis ++ "." ++ "\n\\--------"

    -- TODO: this looks fragile as haskell objects can match this rule if their
    -- names start with "HS" and they are on top of the module hierarchy.
    priority 2 $ (buildPath -/- "HS*.o") %> \obj -> do
        cSrcs   <- interpret target $ getPkgDataList CSrcs
        modules <- interpret target $ getPkgDataList Modules
        let hSrcs = map (replaceEq '.' '/') modules
            cObjs = [ buildPath -/- src -<.> "o" | src <- cSrcs ]
            hObjs = [ buildPath -/- src  <.> "o" | src <- hSrcs ]
        need $ cObjs ++ hObjs
        build $ fullTarget target Ld (cObjs ++ hObjs) [obj]
