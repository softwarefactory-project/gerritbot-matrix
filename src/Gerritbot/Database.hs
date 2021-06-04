{-# LANGUAGE NoImplicitPrelude #-}

module Gerritbot.Database where

import qualified Data.HashTable.IO as HT
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Relude

newtype DB = DB (HT.BasicHashTable Text (Either UTCTime Text))

new :: IO DB
new = DB <$> HT.new

load :: FilePath -> IO DB
load fp = do
  cached <- mapMaybe toValue . lines <$> Text.readFile fp
  DB <$> HT.fromList cached
  where
    toValue :: Text -> Maybe (Text, Either UTCTime Text)
    toValue t = case words t of
      [a, b] -> Just (a, Right b)
      _ -> Nothing

dump :: DB -> FilePath -> IO ()
dump (DB db) fp = do
  kv <- HT.toList db
  Text.writeFile fp (unlines . mapMaybe toLine $ kv)
  where
    toLine :: (Text, Either UTCTime Text) -> Maybe Text
    toLine (name, Right value) = Just $ unwords [name, value]
    toLine (_, Left _) = Nothing

get :: DB -> (Text -> IO (Maybe Text)) -> Text -> IO (Maybe Text)
get (DB db) fetch user = do
  now <- getCurrentTime
  v <- HT.lookup db user
  case v of
    Just (Right userId) -> pure $ Just userId
    Just (Left date) -> do
      if nominalDiffTimeToSeconds (diffUTCTime now date) > 600
        then doFetch now
        else pure Nothing
    Nothing -> doFetch now
  where
    doFetch now = do
      userId <- fetch user
      let v = case userId of
            Just x -> Right x
            Nothing -> Left now
      HT.insert db user v
      pure userId
