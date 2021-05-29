{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Gerrit.Event as Gerrit
import Gerritbot.Main (Channel (..), MatrixEvent (..), formatMessages, getEventRoom, groupEvents)
import Gerritbot.Utils (glob)
import Matrix (RoomID (..))
import Relude
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "unit tests" $ do
  it "match" $ do
    glob "software-factory/*" "software-factory/gerritbot-haskell"
      `shouldBe` True
  it "match config" $ do
    let roomId = "testRoom"
        projects = ["software-factory/*"]
        branches = ["main"]
        channel = Channel {..}
        event = fakeEvent "software-factory/gerritbot-haskell" "main"
     in getEventRoom event channel `shouldBe` Just "testRoom"
  it "group event" $ do
    let events =
          [ MatrixEvent Nothing "ev4" "room3",
            MatrixEvent (Just "foo") "ev2" "room2",
            MatrixEvent (Just "foo") "ev1" "room1",
            MatrixEvent (Just "bar") "ev1" "room1",
            MatrixEvent (Just "foo") "ev1" "room2",
            MatrixEvent Nothing "ev3" "room2",
            MatrixEvent Nothing "ev4" "room2",
            MatrixEvent (Just "foo") "ev2" "room1"
          ]
     in groupEvents events
          `shouldBe` [ (RoomID "room1", Just "bar", ["ev1"]),
                       (RoomID "room1", Just "foo", ["ev1", "ev2"]),
                       (RoomID "room2", Nothing, ["ev3", "ev4"]),
                       (RoomID "room2", Just "foo", ["ev2", "ev1"]),
                       (RoomID "room3", Nothing, ["ev4"])
                     ]
  it "format oneline" $ do
    formatMessages (Just "foo") ["change1"] `shouldBe` "foo proposed: change1"
  it "format multiline" $ do
    formatMessages (Just "foo") ["change1", "change2"] `shouldBe` "foo proposed:\n- change1\n- change2"
  it "format merged" $ do
    formatMessages Nothing ["Merged change1"] `shouldBe` "Merged change1"
  it "format multi merged" $ do
    formatMessages Nothing ["Merged change1", "Merged change2"] `shouldBe` "- Merged change1\n- Merged change2"
  where
    fakeUser :: Gerrit.User
    fakeUser = Gerrit.User (Just "tristan") Nothing Nothing
    fakeChange :: Text -> Text -> Gerrit.Change
    fakeChange changeProject changeBranch =
      let changeSubject = "DNM"
          changeUrl = "http://localhost"
          changeOwner = fakeUser
       in Gerrit.Change {..}
    fakeEvent :: Text -> Text -> Gerrit.ChangeEvent
    fakeEvent changeEventProject branch =
      let changeEventType = Gerrit.PatchsetCreated
          changeEventUploader = Nothing
          changeEventChange = fakeChange changeEventProject branch
          changeEventPatchSet = Gerrit.PatchSet 0 fakeUser
       in Gerrit.ChangeEvent {..}
