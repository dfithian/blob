module Settings where

import Prelude

import Data.Aeson ((.:), (.=), FromJSON, ToJSON, Value, object, parseJSON, toJSON, withObject)
import Data.Text (Text)

data AppSettings = AppSettings
  { appPort     :: Int -- ^ The port to serve the application on.
  , appForceSsl :: Bool -- ^ Whether to force SSL.
  , appConnStr  :: Text -- ^ Database connection string
  }

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \obj ->
    AppSettings
      <$> obj .: "port"
      <*> obj .: "force-ssl"
      <*> obj .: "conn-str"

instance ToJSON AppSettings where
  toJSON AppSettings {..} = object
    [ "port" .= appPort
    , "force-ssl" .= appForceSsl
    , "conn-str" .= appConnStr
    ]

staticSettings :: AppSettings
staticSettings = AppSettings
  { appPort = 8080
  , appForceSsl = False
  , appConnStr = "postgres://postgres@localhost:5432/postgres"
  }

staticSettingsValue :: Value
staticSettingsValue = toJSON staticSettings
