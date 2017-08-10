{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TypeOperators      #-}


module Lib
    ( startApp
    ) where


import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Data
import qualified Data.HashMap.Strict      as M
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.Environment


newtype Info
  = Info
    { environment :: M.HashMap String String
    } deriving (Eq, Show, Data, Typeable, Generic)

instance ToJSON Info

type API = "info" :> Get '[JSON] Info


startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = liftIO getInfo

getInfo :: IO Info
getInfo =   Info
        <$> fmap M.fromList getEnvironment
