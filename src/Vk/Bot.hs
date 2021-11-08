module Vk.Bot where

import qualified Config               as C
import           Control.Concurrent   (threadDelay)
import           Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT))
import           Control.Monad.State  (MonadState (get, put), StateT,
                                       evalStateT)
import           Data.Map.Strict
import           Data.Maybe           (catMaybes)
import           Echo                 (DataLoop (DataLoop), echo)
import           Logger               (LogHandle, logError, logInfo)
import qualified UsersLists           as UL
import           Vk.API               (getLongPollHistory, getTsAndPts)
import           Vk.Impl              (fromItemToUsersMessages)
import           Vk.Responses         (VkItem, VkMessages (vkMessagesItems),
                                       VkResponseType (serverMessages))
import           Vk.Types             (Pts, Ts, VkToken (VkToken))
import           Vk.VkHandle          (vkHandler)

delayTime :: Int
delayTime = 3000000

vkGetUpdates ::
       LogHandle IO -> VkToken -> Maybe (Ts, Pts) -> IO (Maybe [VkItem])
vkGetUpdates hLogger token lastUpdId = do
    vkResponseType <- getLongPollHistory token hLogger lastUpdId
    return $ vkMessagesItems <$> (serverMessages =<< vkResponseType)

updateUpdateId :: VkToken -> LogHandle IO -> StateT (DataLoop (Ts, Pts)) IO ()
updateUpdateId vkToken hLogger = do
    (DataLoop list _) <- get
    nextUpdateId <- liftIO $ getTsAndPts vkToken hLogger
    put $ DataLoop list nextUpdateId

loopBot ::
       LogHandle IO
    -> VkToken
    -> UL.HelpMessage
    -> StateT (DataLoop (Ts, Pts)) IO ()
loopBot hLogger token helpMessage = do
    (DataLoop _ updateId) <- get
    updates <- liftIO $ vkGetUpdates hLogger token updateId
    let usersMessages =
            case updates of
                Nothing   -> []
                Just m_um -> catMaybes $ fromItemToUsersMessages <$> m_um
    let handler = vkHandler hLogger token helpMessage
    --let messages = handlers <$> usersMessages
    mapM_ (runReaderT (echo handler)) usersMessages
    updateUpdateId token hLogger
    liftIO $ threadDelay delayTime
    loopBot hLogger token helpMessage

startBot :: LogHandle IO -> C.BotConfig -> IO ()
startBot hLogger botConf = do
    liftIO $ logInfo hLogger "New Bot Start"
    liftIO $ logInfo hLogger "Check token"
    ch <- liftIO $ getTsAndPts (VkToken (C.token botConf)) hLogger
    case ch of
        Nothing -> liftIO $ logError hLogger "Bad token"
        mark -> do
            liftIO $ logInfo hLogger "Good token"
            evalStateT
                (loopBot
                     hLogger
                     (VkToken (C.token botConf))
                     (UL.HelpMessage $ C.help botConf))
                (DataLoop empty mark)
