{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}


module Lib
    ( startApp
    ) where

-- TODO: publish swagger docs
-- TODO: ekg
-- TODO: db
-- TODO: tests
-- TODO: image version tags
-- TODO: events
-- TODO: deploy with branches and merging


import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Data
import qualified Data.HashMap.Strict      as M
import qualified Data.List                as L
import qualified Data.Text                as T
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger       (withStdoutLogger)
import           Servant
import           System.Environment
import           System.IO


newtype Info
  = Info
    { _environment :: M.HashMap String String
    } deriving (Eq, Show, Data, Typeable, Generic)

instance ToJSON Info where
  toEncoding =
    genericToEncoding $ defaultOptions { fieldLabelModifier = L.drop 1 }

type API = "v1" :> "info" :> Get '[JSON] Info

newtype Action a = Action { runAction :: LoggingT Handler a }
  deriving (Functor, Applicative, Monad, MonadLogger, MonadIO)


startApp :: IO ()
startApp = withStdoutLogger $ \logger -> do
  port     <- maybe 8080 read <$> lookupEnv "PORT"
  logLevel <- maybe LevelError readLogLevel <$> lookupEnv "LOG_LEVEL"
  let settings = setPort port
               $ setLogger logger defaultSettings
  hPutStrLn stderr $ "Starting grivna on port " ++ show port ++ "."
  runSettings settings (app logLevel)

readLogLevel :: String -> LogLevel
readLogLevel "DEBUG" = LevelDebug
readLogLevel "INFO"  = LevelInfo
readLogLevel "WARN"  = LevelWarn
readLogLevel "ERROR" = LevelError
readLogLevel other   = LevelOther $ T.pack other

actionToHandler' :: forall x. LogLevel -> Action x -> Handler x
actionToHandler' level a =
  runStdoutLoggingT (filterLogger lf (runAction a))
    where
      lf :: LogSource -> LogLevel -> Bool
      lf _ msgLevel = msgLevel >= level

actionToHandler :: LogLevel -> (Action :~> Handler)
actionToHandler level = NT (actionToHandler' level)

app :: LogLevel -> Application
app logLevel = serve api (actionServer logLevel)

api :: Proxy API
api = Proxy

actionServer :: LogLevel -> Server API
actionServer level = enter (actionToHandler level) actionServerT

actionServerT :: ServerT API Action
actionServerT = info
  where
    info :: Action Info
    info = do
      logInfoN "Reading environment."
      liftIO getInfo

getInfo :: IO Info
getInfo =   Info
        <$> fmap M.fromList getEnvironment
