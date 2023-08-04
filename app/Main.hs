{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson.KeyMap qualified as KM
import Data.Char
import Data.Foldable
import Data.List (sortOn)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Yaml
import Network.HTTP.Req
import System.Directory
import System.Exit
import System.IO.Temp
import System.Process

import Index
import Log
import RevDeps

getPackagesFromYaml :: IO (Set Text)
getPackagesFromYaml = do
  let config = defaultHttpConfig {httpConfigCheckResponse = \_ _ _ -> Nothing}
  res <-
    runReq config $
      req GET (https "raw.githubusercontent.com" /: "commercialhaskell" /: "stackage" /: "master" /: "build-constraints.yaml") NoReqBody bsResponse mempty
  let yaml = responseBody res
  Object obj <- decodeThrow yaml
  Just (Object packages) <- pure $ KM.lookup "packages" obj
  Just (Array arr) <- pure $ KM.lookup "Library and exe bounds failures" packages
  pure $ S.fromList [T.takeWhile (not . isSpace) xs | String xs <- V.toList arr]

dropVersion :: Text -> Text
dropVersion = T.init . T.dropWhileEnd (/= '-')

tryToBuild :: Log -> Log -> Text -> IO Bool
tryToBuild buildable unbuildable pkgWithVersion = do
  knownBuildable <- elemLog pkgWithVersion buildable
  knownUnbuildable <- elemLog pkgWithVersion unbuildable
  if knownBuildable
    then pure True
    else
      ( if knownUnbuildable
          then pure False
          else withSystemTempDirectory (T.unpack pkgWithVersion) $
            \tmpDir -> withCurrentDirectory tmpDir $ do
              callProcess "cabal" ["get", T.unpack pkgWithVersion]
              withCurrentDirectory (T.unpack pkgWithVersion) $ do
                constraints <- concatMap (\p -> ["-c", T.unpack (dropVersion p) ++ "<0"]) <$> readLog unbuildable
                let crpr = proc "cabal" ("build" : "--allow-newer" : constraints)
                exitCode <- withCreateProcess crpr (\_ _ _ -> waitForProcess)
                let builds = case exitCode of
                      ExitSuccess -> True
                      ExitFailure {} -> False

                appendLog pkgWithVersion (if builds then buildable else unbuildable)
                pure builds
      )

main :: IO ()
main = do
  revDeps <- fetchRevDeps
  packages <- getPackagesFromYaml
  let sortedPackages =
        sortOn (\t -> negate $ M.findWithDefault 0 t revDeps) (S.toList packages)

  index <- readIndex
  let versionedPackages = map (elaborateVersion index) sortedPackages

  buildable <- initLog buildableLog
  unbuildable <- initLog unbuildableLog
  traverse_ (tryToBuild buildable unbuildable) versionedPackages
