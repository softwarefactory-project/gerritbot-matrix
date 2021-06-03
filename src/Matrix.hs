{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | A bare minimal matrix client
module Matrix where

import Control.Monad (mzero)
import Data.Aeson
import Data.ByteString.Lazy.Base64.URL (encodeBase64Unpadded)
import Data.Digest.Pure.SHA (bytestringDigest, sha256)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Relude (Hashable, toLazy, toStrict)

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
  pure $
    initRequest
      { HTTP.requestHeaders =
          [ ("Authorization", "Bearer " <> encodeUtf8 token),
            ("Content-Type", "application/json")
          ]
      }

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

data OpenIDToken = OpenIDToken
  { oiAccessToken :: Text,
    oiTokenType :: Text,
    oiMatrixServerName :: Text,
    oiExpiresIn :: Int
  }
  deriving (Show)

instance FromJSON OpenIDToken where
  parseJSON (Object v) =
    OpenIDToken
      <$> v .: "access_token"
      <*> v .: "token_type"
      <*> v .: "matrix_server_name"
      <*> v .: "expires_in"
  parseJSON _ = mzero

instance ToJSON OpenIDToken where
  toJSON OpenIDToken {..} =
    object
      [ "access_token" .= oiAccessToken,
        "token_type" .= oiTokenType,
        "matrix_server_name" .= oiMatrixServerName,
        "expires_in" .= oiExpiresIn
      ]

getOpenIdToken :: Session -> IO OpenIDToken
getOpenIdToken session = do
  WhoAmI userId <- checkAccount session
  request <- mkRequest session $ "/_matrix/client/r0/user/" <> userId <> "/openid/request_token"
  doRequest
    session
    ( request
        { HTTP.method = "POST",
          HTTP.requestBody = HTTP.RequestBodyLBS "{}" -- otherwise call fails with M_NOT_JSON
        }
    )

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

sendMessage :: Session -> RoomID -> Text -> Text -> Text -> IO EventID
sendMessage session (RoomID roomId) body bodyHTML txnId = do
  request <- mkRequest session $ "/_matrix/client/r0/rooms/" <> roomId <> "/send/m.room.message/" <> txnId
  doRequest
    session
    ( request
        { HTTP.method = "PUT",
          HTTP.requestBody = HTTP.RequestBodyLBS $ encode putData
        }
    )
  where
    putData = MessageEvent "m.notice" body "org.matrix.custom.html" bodyHTML

instance FromJSON RoomID where
  parseJSON (Object v) = RoomID <$> v .: "room_id"
  parseJSON _ = mzero

joinRoom :: Session -> RoomID -> IO RoomID
joinRoom session (RoomID roomId) = do
  request <- mkRequest session $ "/_matrix/client/r0/rooms/" <> roomId <> "/join"
  doRequest session (request {HTTP.method = "POST"})

newtype IdentityToken = IdentityToken Text

instance FromJSON IdentityToken where
  parseJSON (Object v) = IdentityToken <$> v .: "token"
  parseJSON _ = mzero

registerIdentityAccount :: Session -> OpenIDToken -> IO IdentityToken
registerIdentityAccount session openIDToken = do
  request <- mkRequest session "/_matrix/identity/v2/account/register"
  doRequest
    session
    ( request
        { HTTP.method = "POST",
          HTTP.requestBody = HTTP.RequestBodyLBS $ encode openIDToken
        }
    )

acceptPolicies :: Session -> [Text] -> IO Value
acceptPolicies session policies = do
  request <- mkRequest session "/_matrix/identity/v2/terms"
  doRequest
    session
    ( request
        { HTTP.method = "POST",
          HTTP.requestBody = HTTP.RequestBodyLBS $ encode $ object ["user_accepts" .= policies]
        }
    )

createIdentitySession :: Session -> OpenIDToken -> IO Session
createIdentitySession session openIDToken = do
  IdentityToken token <- registerIdentityAccount session openIDToken
  pure $ session {token = token}

data HashDetails = HashDetails
  { hdAlgorithms :: [Text],
    hdPepper :: Text
  }
  deriving (Show)

instance FromJSON HashDetails where
  parseJSON (Object v) = HashDetails <$> v .: "algorithms" <*> v .: "lookup_pepper"
  parseJSON _ = mzero

checkIdentityAccount :: Session -> IO WhoAmI
checkIdentityAccount session = do
  request <- mkRequest session "/_matrix/identity/v2/account"
  doRequest session request

hashDetails :: Session -> IO HashDetails
hashDetails session = do
  request <- mkRequest session "/_matrix/identity/v2/hash_details"
  doRequest session request

newtype Email = Email Text deriving (Show)

newtype UserID = UserID Text deriving (Show)

encodeSHA256 :: Text -> Text
encodeSHA256 = toStrict . encodeBase64Unpadded . bytestringDigest . sha256 . toLazy . encodeUtf8

identityLookup :: Session -> HashDetails -> Email -> IO (Maybe UserID)
identityLookup session hashDetails' (Email email) = do
  request <- mkRequest session "/_matrix/identity/v2/lookup"
  lookupUser
    <$> doRequest
      session
      ( request
          { HTTP.method = "POST",
            HTTP.requestBody = HTTP.RequestBodyLBS body
          }
      )
  where
    lookupUser resp = case resp of
      Object kv -> case HM.lookup "mappings" kv of
        Just (Object kv') -> case HM.lookup address kv' of
          Just (String v) -> Just (UserID v)
          _ -> Nothing
        _ -> Nothing
      _ -> Nothing
    body =
      encode $
        object
          [ "addresses" .= [address],
            "algorithm" .= head (hdAlgorithms hashDetails'),
            "pepper" .= hdPepper hashDetails'
          ]
    address = encodeSHA256 (email <> " email " <> hdPepper hashDetails')
