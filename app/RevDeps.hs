{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module RevDeps where

import Data.ByteString.Char8 qualified as B
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Req
import Text.HTML.TagSoup

fetchRevDeps :: IO (Map Text Int)
fetchRevDeps = do
  let config = defaultHttpConfig {httpConfigCheckResponse = \_ _ _ -> Nothing}
  res <-
    runReq config $
      req GET (https "packdeps.haskellers.com" /: "reverse") NoReqBody bsResponse mempty
  let html = parseTags $ B.unpack $ responseBody res
      trs = drop 1 $ partitions (isTagOpenName "tr") html
      extractPackage = \case
        TagOpen "tr" [] : TagOpen "td" [] : TagOpen "a" _ : TagText txt : TagClose "a" : TagText "\n" : TagClose "td" : TagText "\n" : TagOpen "td" [] : TagText cnt : _ -> Just (T.pack txt, read cnt)
        _ -> Nothing
  pure $ M.fromList $ mapMaybe extractPackage trs
