module Types where

import Prelude

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Servant.API (FromHttpApiData, ToHttpApiData)

import Json (jsonOptions)

newtype BlobId = BlobId { unBlobId :: Text }
  deriving (Eq, Ord, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, FromField, ToField)

newtype BlobPin = BlobPin { unBlobPin :: Text }
  deriving (Eq, Ord, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, FromField, ToField)

newtype Blob = Blob { unBlob :: Text }
  deriving (Eq, Ord, Show, FromJSON, ToJSON, FromField, ToField)

data PutBlobRequest = PutBlobRequest
  { putBlobRequestPin :: BlobPin
  , putBlobRequestBlob :: Blob
  }
  deriving (Eq, Ord, Show)

data GetBlobResponse = GetBlobResponse
  { getBlobResponseBlob :: Blob
  , getBlobResponseModifiedAt :: UTCTime
  }

deriveJSON (jsonOptions "putBlobRequest") ''PutBlobRequest
deriveJSON (jsonOptions "getBlobResponse") ''GetBlobResponse
