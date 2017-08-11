{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TypeOperators      #-}


module Lib
    ( startApp
    ) where

-- TODO: logging
-- TODO: publish swagger docs
-- TODO: versioning


import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Data
import qualified Data.HashMap.Strict      as M
import qualified Data.List                as L
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
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

type API = "info" :> Get '[JSON] Info


startApp :: IO ()
startApp = do
  port <- maybe 8080 read <$> lookupEnv "PORT"
  hPutStrLn stderr $ "Starting grivna on port " ++ show port ++ "."
  run port app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = liftIO getInfo

getInfo :: IO Info
getInfo =   Info
        <$> fmap M.fromList getEnvironment
