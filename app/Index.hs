{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Index where

import Cabal.Config
import Cabal.Index
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.Client.Config (loadConfig, savedGlobalFlags)
import Distribution.Client.GlobalFlags (globalCacheDir)
import Distribution.Simple.Flag (fromFlag)
import Distribution.Types.PackageName
import Distribution.Types.Version
import System.FilePath ((</>))

readIndex :: IO (Map PackageName PackageInfo)
readIndex = do
  cnf <- loadConfig minBound mempty
  let cacheDir = fromFlag $ globalCacheDir $ savedGlobalFlags cnf
      indexPath = cacheDir </> hackageHaskellOrg </> "01-index.tar"
  indexMetadata indexPath Nothing

elaborateVersion :: Map PackageName PackageInfo -> Text -> Text
elaborateVersion index txt = case M.lookup (mkPackageName (T.unpack txt)) index of
  Nothing -> error $ T.unpack txt ++ " not found in index"
  Just (PackageInfo vers _) -> txt <> "-" <> T.intercalate "." (map (T.pack . show) (versionNumbers (fst (M.findMax vers))))
