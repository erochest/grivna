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

-- TODO: ekg
-- TODO: db
-- TODO: tests
-- TODO: image version tags
-- TODO: events
-- TODO: deploy with branches and merging
-- TODO: refactor


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
import Servant.Swagger
import Data.Swagger hiding (Info, fieldLabelModifier)


newtype Info
  = Info
    { _environment :: M.HashMap String String
    } deriving (Eq, Show, Data, Typeable, Generic)

instance ToJSON Info where
  toEncoding =
    genericToEncoding $ defaultOptions { fieldLabelModifier = L.drop 1 }

instance ToSchema Info

type API1 =    "info"    :> Get '[JSON] Info
          :<|> "version" :> Get '[JSON] T.Text

type API = "v1" :>
  (    "api"     :> API1
  :<|> "swagger" :> Get '[JSON] Swagger
  )

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

api1 :: Proxy API1
api1 = Proxy

api :: Proxy API
api = Proxy

actionServer :: LogLevel -> Server API
actionServer level = enter (actionToHandler level) actionServerT

actionServerT :: ServerT API Action
actionServerT = (info :<|> version) :<|> swagger
  where
    info :: Action Info
    info = do
      logInfoN "Reading environment."
      liftIO getInfo

    version :: Action T.Text
    version = return "swagger"

    swagger :: Action Swagger
    swagger = return $ toSwagger api1

getInfo :: IO Info
getInfo =   Info
        <$> fmap M.fromList getEnvironment
