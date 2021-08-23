
module Lib where
{-import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Simple
import qualified Data.Text.Encoding as E
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import API
import TelegramResponses
import Data.Aeson
import GHC.Generics
import Network.HTTP.Req
import Control.Monad.IO.Class

{-someFunc :: IO ()
someFunc = putStrLn "someFunc"


myToken :: BC.ByteString
myToken = "1431530804:AAH5bSr9xr8o3WQlF55hnmpYZYtktn-rzWY"


rusText :: BC.ByteString
rusText = "Привет"

engText :: BC.ByteString
engText = "Hello"

sendMessageMethod:: BC.ByteString
sendMessageMethod = "/sendMessage?chat_id="

chatid :: BC.ByteString
chatid = "274864287&text="

sendMessageTest :: BC.ByteString
sendMessageTest = BC.concat ["https://api.telegram.org/bot", myToken,sendMessageMethod,chatid]

rus :: BC.ByteString
rus = BC.concat [sendMessageTest,rusText]

eng :: BC.ByteString
eng = BC.concat [sendMessageTest,engText]


rus' = parseRequest_ $ BC.unpack rus
eng' = parseRequest_ $ BC.unpack eng
sendMessage :: IO BC.ByteString
sendMessage = do
    res <- httpBS rus'
    res' <- httpBS eng'
    return (getResponseBody res)
---------------------------------------------------------------------------------------}-}





