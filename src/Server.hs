module Server where

import Prelude

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logError)
import Data.Time.Clock (getCurrentTime)
import Database.PostgreSQL.Simple (Only(..), execute, query)
import Servant.Server (err400, err404, err500)

import Foundation (AppM, tshow, withDb)
import Types (GetBlobResponse(..), PutBlobRequest(..), BlobId, BlobPin)

putBlob :: AppM m => BlobId -> PutBlobRequest -> m ()
putBlob blobId PutBlobRequest {..} = do
  now <- liftIO getCurrentTime
  inserted <- withDb $ \c ->
    query c "select blob_pin from blob.blobs where blob_id = ?" (Only blobId) >>= \case
      [Only pin] -> case pin == putBlobRequestPin of
        True -> True <$ execute c "update blob.blobs set blob = ?, modified_at = ? where blob_id = ?" (putBlobRequestBlob, now, blobId)
        False -> pure False
      [] -> True <$ execute c "insert into blob.blobs (blob_id, blob_pin, blob, modified_at) values (?, ?, ?, ?)" (blobId, putBlobRequestPin, putBlobRequestBlob, now)
      _ -> pure False
  unless inserted $ throwError err400

getBlob :: AppM m => Maybe BlobPin -> BlobId -> m GetBlobResponse
getBlob blobPinMay blobId = do
  blobPin <- case blobPinMay of
    Nothing -> throwError err404
    Just p -> pure p
  withDb (\c -> query c "select blob_pin, blob, modified_at from blob.blobs where blob_id = ?" (Only blobId)) >>= \case
    [] -> throwError err404
    [(pin, blob, modifiedAt)] -> do
      $logError $ tshow (blob, pin)
      case pin == blobPin of
        True -> pure $ GetBlobResponse
          { getBlobResponseBlob = blob
          , getBlobResponseModifiedAt = modifiedAt
          }
        False -> throwError err404
    xs -> do
      $logError $ "Too many blobs for " <> tshow blobId <> ": " <> tshow xs
      throwError err500
