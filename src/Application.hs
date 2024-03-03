module Application where

import Prelude

import Data.Pool (defaultPoolConfig, newPool)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Network.Wai.Handler.Warp (Settings, defaultSettings, runSettings, setPort)
import Network.Wai.Middleware.RequestLogger
  ( OutputFormat(Detailed), defaultRequestLoggerSettings, mkRequestLogger, outputFormat
  )
import Servant.API ((:<|>)((:<|>)))
import Servant.Server (ServerT, hoistServer, serve)
import qualified Network.Wai.Middleware.EnforceHTTPS as EnforceHTTPS

import Foundation (App(..), Server, runServer)
import Servant (Api, api)
import Server (getBlob, putBlob)
import Settings (AppSettings(..), staticSettingsValue)

server :: ServerT Api Server
server = putBlob :<|> getBlob

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
  appPool <- newPool (defaultPoolConfig (connectPostgreSQL (encodeUtf8 (appConnStr appSettings))) close 30 10)
  pure App {..}

warpSettings :: App -> Settings
warpSettings app =
  setPort (appPort $ appSettings app)
    $ defaultSettings

appMain :: IO ()
appMain = do
  settings <- loadYamlSettingsArgs [staticSettingsValue] useEnv
  app <- makeFoundation settings
  let ssl = case appForceSsl settings of
        True -> EnforceHTTPS.withResolver EnforceHTTPS.xForwardedProto
        False -> id
      appl = serve api $ hoistServer api (runServer app) server
  requestLogger <- mkRequestLogger (defaultRequestLoggerSettings { outputFormat = Detailed False })
  runSettings (warpSettings app) $ requestLogger $ ssl appl
