module VkResponses where
import Data.Aeson
import GHC.Generics
import Control.Monad

data VkResponse = VkResponse { someResponse :: VkResponseType
                            } deriving (Show, Generic)
instance FromJSON VkResponse where
    parseJSON (Object v) = 
        VkResponse <$> v .: "response"

data VkResponseType = Server { serverServer :: String
                               , serverKey :: String
                               , serverTS :: Int
                               , serverPTS :: Int
                               } deriving (Show, Generic)
instance FromJSON VkResponseType where
    parseJSON (Object v) = 
        Server <$> v .: "server"
               <*> v .: "key"
               <*> v .: "ts"
               <*> v .: "pts"

