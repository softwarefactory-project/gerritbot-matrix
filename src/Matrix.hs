{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | A bare minimal matrix client
module Matrix where

import Control.Monad (mzero)
import Data.Aeson
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Relude (Hashable, toLazy)

data Session = Session
  { baseUrl :: Text,
    token :: Text,
    manager :: HTTP.Manager
  }

createSession :: Text -> Text -> IO Session
createSession baseUrl token = Session baseUrl token <$> HTTP.newManager tlsManagerSettings

mkRequest :: Session -> Text -> IO HTTP.Request
mkRequest Session {..} path = do
  initRequest <- HTTP.parseUrlThrow (unpack $ baseUrl <> path)
  pure $ HTTP.setQueryString [("access_token", Just $ encodeUtf8 token)] initRequest

doRequest :: FromJSON a => Session -> HTTP.Request -> IO a
doRequest Session {..} request = do
  response <- HTTP.httpLbs request manager
  case eitherDecode (HTTP.responseBody response) of
    Right a -> pure a
    Left err -> error err

newtype WhoAmI = WhoAmI Text deriving (Show)

instance FromJSON WhoAmI where
  parseJSON (Object v) = WhoAmI <$> v .: "user_id"
  parseJSON _ = mzero

checkAccount :: Session -> IO WhoAmI
checkAccount session = do
  request <- mkRequest session "/_matrix/client/r0/account/whoami"
  doRequest session request

newtype RoomID = RoomID Text
  deriving (Show, Eq)
  deriving newtype (Hashable)

newtype EventID = EventID Text deriving (Show)

instance FromJSON EventID where
  parseJSON (Object v) = EventID <$> v .: "event_id"
  parseJSON _ = mzero

data MessageEvent = MessageEvent
  { meMsgtype :: Text,
    meBody :: Text,
    meFormat :: Text,
    meFormattedBody :: Text
  }
  deriving (Show)

instance ToJSON MessageEvent where
  toJSON msg =
    object
      [ "msgtype" .= meMsgtype msg,
        "body" .= meBody msg,
        "format" .= meFormat msg,
        "formatted_body" .= meFormattedBody msg
      ]

sendMessage :: Session -> RoomID -> Text -> Text -> IO EventID
sendMessage session (RoomID roomId) body bodyHTML = do
  request <- mkRequest session $ "/_matrix/client/r0/rooms/" <> roomId <> "/send/m.room.message/" <> ctx
  doRequest
    session
    ( request
        { HTTP.method = "PUT",
          HTTP.requestBody = HTTP.RequestBodyLBS $ encode putData
        }
    )
  where
    ctx = pack . showDigest . sha1 . toLazy . encodeUtf8 $ body
    putData = MessageEvent "m.notice" body "org.matrix.custom.html" bodyHTML
