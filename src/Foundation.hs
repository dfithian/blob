module Foundation where

import Prelude

import Control.Monad.Catch (MonadCatch, SomeException, catch, throwM)
import Control.Monad.Except (ExceptT, MonadError, mapExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
  ( Loc, LogLevel, LogSource, LogStr, LoggingT, MonadLogger, logError, runStdoutLoggingT
  )
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.Pool (Pool, withResource)
import Data.Text (Text, pack)
import Database.PostgreSQL.Simple (Connection)
import Servant.Server (Handler(Handler), ServerError, err500)

import Settings (AppSettings)

type AppM m = (MonadCatch m, MonadError ServerError m, MonadIO m, MonadLogger m, MonadReader App m)

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

type Server = ExceptT ServerError (ReaderT App (LoggingT IO))

data App = App
  { appSettings :: AppSettings -- ^ The settings for the app.
  , appPool     :: Pool Connection
  }

class HasSettings a where
  settings :: a -> AppSettings

instance HasSettings App where
  settings = appSettings

instance HasSettings AppSettings where
  settings = id

withDb :: AppM m => (Connection -> IO a) -> m a
withDb f = do
  App {..} <- ask
  liftIO (withResource appPool f) `catch` \(se :: SomeException) -> do
    $logError (tshow se)
    throwError err500

tshow :: Show a => a -> Text
tshow = pack . show

logErrors :: (MonadCatch m, MonadIO m) => m a -> m a
logErrors ma = do
  ma `catch` \(se :: SomeException) -> do
    runStdoutLoggingT ($logError (tshow se))
    throwM se

runServer :: App -> Server a -> Handler a
runServer app ma = Handler (mapExceptT (\ma' -> runStdoutLoggingT (runReaderT ma' app)) ma)
