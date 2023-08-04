{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Index where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T

import Cabal.Config
import Cabal.Index
import Distribution.Types.PackageName
import Distribution.Types.Version

readIndex :: IO (Map PackageName PackageInfo)
readIndex = do
  cfg <- readConfig
  indexPath <-
    maybe
      (error "Cannot find 01.index.tar")
      pure
      (cfgRepoIndex cfg hackageHaskellOrg)
  indexMetadata indexPath Nothing

elaborateVersion :: Map PackageName PackageInfo -> Text -> Text
elaborateVersion index txt = case M.lookup (mkPackageName (T.unpack txt)) index of
  Nothing -> error $ T.unpack txt ++ " not found in index"
  Just (PackageInfo vers _) -> txt <> "-" <> T.intercalate "." (map (T.pack . show) (versionNumbers (fst (M.findMax vers))))
