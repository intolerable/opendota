module RunVCRClient where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Servant.Client
import Servant.Client.Core
import Control.Monad.Trans.State
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Network.HTTP.Types.Version

data VCRFixture = VCRFixture
  { fixtureRequest :: RequestF ByteString ByteString
  , fixtureResponse :: ResponseF ByteString
  } deriving (Show, Eq)

instance FromJSON VCRFixture where
  parseJSON = withObject "VCRFixture" \o -> do
    VCRFixture <$> (o .: "request" >>= requestFromJSON)
               <*> (o .: "response" >>= responseFromJSON)

requestFromJSON :: Object -> Parser (RequestF ByteString ByteString)
requestFromJSON o =
  Request <$> (ByteString.pack <$> o .: "path")
          <*> pure mempty
          <*> pure Nothing
          <*> pure mempty
          <*> pure mempty
          <*> pure http11
          <*> undefined -- o .: "method"

responseFromJSON :: Value -> Parser (ResponseF ByteString)
responseFromJSON = undefined

instance ToJSON VCRFixture where
  toJSON VCRFixture{..} =
    object
      [ "request" .= requestToJSON fixtureRequest
      , "response" .= responseToJSON fixtureResponse
      ]

requestToJSON :: RequestF ByteString ByteString -> Value
requestToJSON Request{..} =
  object
    [ "path" .= ByteString.unpack requestPath
    ]

responseToJSON :: ResponseF ByteString -> Value
responseToJSON = undefined

data VCRState = VCRState
  { fixtures :: [VCRFixture]
  , matchFunction :: RequestF ByteString ByteString -> RequestF ByteString ByteString -> Bool
  }

newtype RunVCRClient m a = RunVCRClient (StateT VCRState m a)
  deriving (Functor, Applicative, Monad)

instance RunClient m => RunClient (RunVCRClient m) where
  runRequestAcceptStatus = undefined
  throwClientError = undefined
