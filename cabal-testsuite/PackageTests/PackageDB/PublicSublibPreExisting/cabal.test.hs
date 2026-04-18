import Test.Cabal.Prelude

main :: IO ()
main = cabalTest . recordMode DoNotRecord . noCabalPackageDb $ withPackageDb $ do
  withDirectory "dep" $ setup_install []

  -- The scenario only makes sense if both the main library and the public
  -- sublibrary were registered in the external package DB.
  ghcPkg' "field" ["db-public-sublib-dep", "id", "--simple-output"]
    >>= assertOutputContains "db-public-sublib-dep"
  ghcPkg' "field" ["z-db-public-sublib-dep-z-visible", "id", "--simple-output"]
    >>= assertOutputContains "db-public-sublib-dep"

  env <- getTestEnv
  let pkgDbPath = testPackageDbDir env

  withDirectory "q" $ do
    cabal "v2-build"
      [ "--package-db=clear"
      , "--package-db=global"
      , "--package-db=" ++ pkgDbPath
      ]
    withPlan $
      runPlanExe' "db-public-sublib-consumer" "consumer" []
        >>= assertOutputContains "main-lib via visible"
