{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Gerrit.Event as Gerrit
import Gerritbot.Main hiding (main)
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
        events = [PatchsetCreated]
        channel = Channel {..}
        change = fakeChange "software-factory/gerritbot-haskell" "main"
     in getEventRoom Gerrit.PatchsetCreatedEvent change channel `shouldBe` Just (RoomID "testRoom")
  it "group event" $ do
    let events =
          [ -- Sequencial events
            MatrixEvent action1 object1 room1,
            MatrixEvent action1 object2 room1,
            -- Interleaved events
            MatrixEvent action2 object1 room2,
            MatrixEvent action2 object1 room1,
            MatrixEvent action2 object2 room2,
            MatrixEvent action2 object2 room1,
            -- Out of order
            MatrixEvent action3 object2 room1,
            MatrixEvent action3 object1 room1
          ]
     in groupEvents events
          `shouldBe` [ (room1, action2, [object1, object2]),
                       (room1, action1, [object1, object2]),
                       (room1, action3, [object2, object1]),
                       (room2, action2, [object1, object2])
                     ]
  it "format oneline" $ do
    formatMessages action1 [object1] `shouldBe` "foo proposed: change1"
  it "format multiline" $ do
    formatMessages action1 [object1, object2] `shouldBe` "foo proposed:\n- change1\n- change2"
  where
    (action1, action2, action3) =
      ( EventAction "foo proposed:",
        EventAction "foo merge:",
        EventAction "bar do:"
      )
    (object1, object2) =
      (EventObject "change1", EventObject "change2")
    (room1, room2) =
      (RoomID "room1", RoomID "room2")
    fakeUser :: Gerrit.User
    fakeUser = Gerrit.User (Just "tristan") Nothing Nothing
    fakeChange :: Text -> Text -> Gerrit.Change
    fakeChange changeProject changeBranch =
      let changeSubject = "DNM"
          changeUrl = "http://localhost"
          changeOwner = fakeUser
       in Gerrit.Change {..}
