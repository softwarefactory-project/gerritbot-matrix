{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The gerritbot-matrix main entrypoint
module Main where

import qualified Data.Text as Text
import Dhall
import qualified Dhall.TH
import qualified Gerrit.Event as Gerrit
import qualified Gerritbot
import qualified Matrix
import Options.Generic
import Relude

data CLI w = CLI
  { gerritHost :: w ::: Text <?> "The gerrit host",
    gerritUser :: w ::: Text <?> "The gerrit username",
    matrixUrl :: w ::: Text <?> "The matrix url",
    configFile :: w ::: FilePath <?> "The gerritbot.dhall path"
  }
  deriving stock (Generic)

instance ParseRecord (CLI Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

Dhall.TH.makeHaskellTypes [Dhall.TH.SingleConstructor "Channel" "Channel" "(./src/Config.dhall).Type"]

match :: Text -> Text -> Bool
match obj conf = conf `Text.isPrefixOf` obj

formatMessage :: Gerrit.ChangeEvent -> Text
formatMessage Gerrit.ChangeEvent {..} =
  author <> " proposed " <> project <> " " <> branch <> ": " <> url
  where
    url = Gerrit.changeUrl changeEventChange
    branch = Gerrit.changeBranch changeEventChange
    project = changeEventProject
    author = case changeEventUploader of
      Nothing -> "Unknown User"
      Just user ->
        fromMaybe
          "Anonymous User"
          (Gerrit.userName user <|> Gerrit.userUsername user <|> Gerrit.userEmail user)

getEventRoom :: Gerrit.ChangeEvent -> Channel -> Maybe Text
getEventRoom Gerrit.ChangeEvent {..} Channel {..}
  | projectMatch && branchMatch = Just roomId
  | otherwise = Nothing
  where
    projectMatch = any (match changeEventProject) projects
    branchMatch = any (match $ Gerrit.changeBranch changeEventChange) branches

onEvent :: [Channel] -> Matrix.Session -> Gerrit.Event -> IO ()
onEvent channels sess event = case event of
  Gerrit.EventChange changeEvent@Gerrit.ChangeEvent {..} -> do
    putTextLn $ "Processing " <> show event
    case mapMaybe (getEventRoom changeEvent) channels of
      [] -> putTextLn "No channel matched"
      xs -> do
        putTextLn $ "Sending notification to " <> show xs
        mapM_ (sendMessage changeEvent) xs
  _ -> pure mempty
  where
    sendMessage :: Gerrit.ChangeEvent -> Text -> IO ()
    sendMessage changeEvent roomId = do
      res <- Matrix.sendMessage sess (Matrix.RoomID roomId) (formatMessage changeEvent)
      print res

main :: IO ()
main = do
  args <- unwrapRecord "Gerritbot Matrix"
  token <- fromMaybe (error "Missing MATRIX_TOKEN environment") <$> lookupEnv "MATRIX_TOKEN"
  sess <- Matrix.createSession (matrixUrl args) $! toText token
  channels <- Dhall.input auto (toText $ configFile args)
  Gerritbot.runStreamClient
    (Gerritbot.GerritServer (gerritHost args) (gerritUser args))
    (onEvent channels sess)
  putTextLn "Done."
