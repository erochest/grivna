{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Lib
    ( startApp
    ) where

-- TODO: refactor API (remove api)
-- TODO: db
-- TODO: tests
-- TODO: image version tags
-- TODO: events
-- TODO: ekg to cf metrics
-- TODO: deploy with branches and merging
-- TODO: refactor


import           Data.Aeson.Types
import           Data.Data
import qualified Data.HashMap.Strict          as M
import qualified Data.List                    as L
import           Data.Swagger                 hiding (Info, fieldLabelModifier,
                                               info, version)
import qualified Data.Swagger.Schema          as S
import qualified Data.Text                    as T
import           Magicbane
import           Network.Wai.Handler.Warp
import           Servant.Swagger
import           System.Environment
import           System.Remote.Monitoring.Wai (serverMetricStore)


newtype Info
  = Info
    { _environment :: M.HashMap String String
    } deriving (Eq, Show, Data, Typeable, Generic)

instance ToJSON Info where
  toEncoding =
    genericToEncoding $ defaultOptions { fieldLabelModifier = L.drop 1 }

instance ToSchema Info where
  declareNamedSchema =
    genericDeclareNamedSchema (S.defaultSchemaOptions
                                { S.fieldLabelModifier = L.drop 1})

type InfoRoute    = "info"    :> Get '[JSON] Info
type VersionRoute = "version" :> Get '[JSON] T.Text
type SwaggerRoute = "swagger" :> Get '[JSON] Swagger

type API1  = "api" :> (InfoRoute :<|> VersionRoute)
type API1' = "v1"  :> API1
type API   = "v1"  :> (API1 :<|> SwaggerRoute)

api :: Proxy API
api = Proxy

data GrivnaConf
  = GrivnaConf
    { port     :: !Int
    , logLevel :: !LogLevel
    } deriving (Show, Eq, Data, Typeable, Generic)

instance Default GrivnaConf where
  def = GrivnaConf 8080 LevelError

instance FromEnv GrivnaConf

deriving instance Data LogLevel

instance Var LogLevel where
  toVar LevelDebug         = "DEBUG"
  toVar LevelInfo          = "INFO"
  toVar LevelWarn          = "WARN"
  toVar LevelError         = "ERROR"
  toVar (LevelOther other) = T.unpack other

  fromVar "DEBUG" = Just LevelDebug
  fromVar "INFO"  = Just LevelInfo
  fromVar "WARN"  = Just LevelWarn
  fromVar "ERROR" = Just LevelDebug
  fromVar other   = Just $ LevelOther $ T.pack other

type GrivnaContext = (ModLogger, ModMetrics, GrivnaConf)
type GrivnaApp     = MagicbaneApp GrivnaContext

startApp :: IO ()
startApp = withEnvConfig $ \(config :: GrivnaConf) -> do
  (_, logger) <-  second (ModLogger . filterLogging (logLevel config))
              <$> newLogger (LogStderr defaultBufSize)
  let mainPort    = Lib.port config
  metr <- startMetrics $ mainPort + 1
  let context     = (logger, metr, config)
  run mainPort $ magicbaneApp api EmptyContext context actions

filterLogging :: LogLevel -> ModLogger
              -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
filterLogging target (ModLogger f) loc src level str
  | target <= level = f loc src level str
  | otherwise       = return ()

startMetrics :: Int -> IO ModMetrics
startMetrics portno =
  newMetricsWith =<< serverMetricStore <$> forkMetricsServer "0.0.0.0" portno

actions :: (GrivnaApp Info :<|> GrivnaApp T.Text) :<|> GrivnaApp Swagger
actions = (info :<|> version) :<|> swagger

info :: GrivnaApp Info
info = timed "GET info" $ do
  $logInfo$ "Reading environment."
  liftIO getInfo

version :: GrivnaApp T.Text
version = return "ekg"

swagger :: GrivnaApp Swagger
swagger = return $ toSwagger (Proxy :: Proxy API1')

getInfo :: IO Info
getInfo = Info <$> fmap M.fromList getEnvironment
