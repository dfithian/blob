module Servant where

import Data.Proxy (Proxy(Proxy))
import Servant.API ((:<|>), (:>), Capture, Get, Header, JSON, Put, ReqBody)

import Types (BlobId, BlobPin, GetBlobResponse, PutBlobRequest)

api :: Proxy Api
api = Proxy

type Api =
  "api" :> "v1" :> "blob" :> Capture "blob_id" BlobId :> ReqBody '[JSON] PutBlobRequest :> Put '[JSON] ()
    :<|> "api" :> "v1" :> "blob" :> Header "X-Blob-Pin" BlobPin :> Capture "blob_id" BlobId :> Get '[JSON] GetBlobResponse
