{-# LANGUAGE NoImplicitPrelude #-}

module Gerritbot.Database where

import qualified Data.HashTable.IO as HT
import qualified Data.Text.IO as Text
import Relude

newtype DB = DB (HT.BasicHashTable Text (Maybe Text))

new :: IO DB
new = DB <$> HT.new

load :: FilePath -> IO DB
load fp = do
  cached <- mapMaybe toValue . lines <$> Text.readFile fp
  DB <$> HT.fromList cached
  where
    toValue :: Text -> Maybe (Text, Maybe Text)
    toValue t = case words t of
      [a] -> Just (a, Nothing)
      [a, b] -> Just (a, Just b)
      _ -> Nothing

dump :: DB -> FilePath -> IO ()
dump (DB db) fp = do
  kv <- HT.toList db
  Text.writeFile fp (unlines . fmap toLine $ kv)
  where
    toLine :: (Text, Maybe Text) -> Text
    toLine (name, value) = unwords [name, fromMaybe mempty value]

get :: DB -> (Text -> IO (Maybe Text)) -> Text -> IO (Maybe Text)
get (DB db) fetch user = do
  v <- HT.lookup db user
  case v of
    Just userId -> pure userId
    Nothing -> do
      userId <- fetch user
      HT.insert db user userId
      pure userId
