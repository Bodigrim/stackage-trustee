{-# LANGUAGE OverloadedStrings #-}

module Log where

import Data.IORef
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory

buildableLog :: FilePath
buildableLog = "buildable.log"

unbuildableLog :: FilePath
unbuildableLog = "unbuildable.log"

data Log = Log FilePath (IORef (Set Text))

initLog :: FilePath -> IO Log
initLog fn' = do
  fn <- makeAbsolute fn'
  exists <- doesFileExist fn
  xs <- case exists of
    True -> S.fromList . T.lines <$> T.readFile fn
    False -> pure mempty
  Log fn <$> newIORef xs

readLog :: Log -> IO (Set (Text))
readLog (Log _ ref) = readIORef ref

elemLog :: Text -> Log -> IO Bool
elemLog x (Log _ ref) = do
  xs <- readIORef ref
  pure $ x `elem` xs

appendLog :: Text -> Log -> IO ()
appendLog x (Log fn ref) = do
  modifyIORef ref (S.insert x)
  T.appendFile fn (x <> "\n")
