module Echo where

import qualified Config                  as C
import           Logger                  (LogHandle, logError, logInfo)
import qualified Telegram.Answers        as TA
import           Telegram.Responses      (TelegramUpdate)
import qualified Telegram.TelegramHandle as TH
import           Telegram.Types          (TelegramToken, UpdateId)
import           UsersLists              (HelpMessage (HelpMessage),
                                          RepeatsList, updateListUsers)
import qualified Vk.Answers              as VA
import           Vk.Responses            (VkResponseType)
import           Vk.Types                (Pts, Ts, VkToken)
import qualified Vk.VkHandle             as VH

data EchoBotHandle m lastUpdMark updateInf =
    EchoBotHandle
        { getUpdates :: LogHandle m -> Maybe lastUpdMark -> m (Maybe updateInf)
        , answers :: LogHandle m -> HelpMessage -> Maybe updateInf -> RepeatsList -> m RepeatsList
        , getLastUpdateMark :: Maybe updateInf -> LogHandle m -> m (Maybe lastUpdMark)
        , delayFunction :: m ()
        , listOfUsersUpdate :: RepeatsList -> RepeatsList -> RepeatsList
        , checkAvalible :: LogHandle m -> m (Maybe lastUpdMark)
        }

echo ::
       Monad m
    => EchoBotHandle m lastUpdMark updateInf
    -> LogHandle m
    -> HelpMessage
    -> (Maybe lastUpdMark, RepeatsList)
    -> m (Maybe lastUpdMark, RepeatsList)
echo echoHandler hLogger helpMessage (lastUpdMark, reapeatsList) = do
    upd <- getUpdates echoHandler hLogger lastUpdMark
    listOfUsersUpd <- answers echoHandler hLogger helpMessage upd reapeatsList
    let newListOfUsers =
            listOfUsersUpdate echoHandler reapeatsList listOfUsersUpd
    nextUpdateMark <- getLastUpdateMark echoHandler upd hLogger
    delayFunction echoHandler
    return (nextUpdateMark, newListOfUsers)

loopBot ::
       Monad m
    => LogHandle m
    -> EchoBotHandle m lastUpdMark updateInf
    -> HelpMessage
    -> (Maybe lastUpdMark, RepeatsList)
    -> m b
loopBot hLogger handler helpMessage (updMark, listOfRepeats) = do
    nextLoopInformation <-
        echo handler hLogger helpMessage (updMark, listOfRepeats)
    loopBot hLogger handler helpMessage nextLoopInformation

telegramEchoHandler ::
       Monad m
    => TelegramToken
    -> TH.TelegramHandle m
    -> m ()
    -> EchoBotHandle m UpdateId [TelegramUpdate]
telegramEchoHandler telegramToken telegramHandle delayFunc =
    EchoBotHandle
        { getUpdates = TH.getUpdates telegramHandle telegramToken
        , answers = TA.answers telegramHandle telegramToken
        , getLastUpdateMark = TH.getLastUpdateId telegramHandle
        , delayFunction = delayFunc
        , listOfUsersUpdate = updateListUsers
        , checkAvalible = TH.getMe telegramHandle telegramToken
        }

vkEchoHandler ::
       Monad m
    => VkToken
    -> VH.VKHandle m
    -> m ()
    -> EchoBotHandle m (Ts, Pts) VkResponseType
vkEchoHandler vkToken vkHandle delayFunc =
    EchoBotHandle
        { getUpdates = VH.getLongPollHistory vkHandle vkToken
        , answers = VA.answers vkHandle vkToken
        , getLastUpdateMark = \_ -> VH.getTsAndPts vkHandle vkToken
        , delayFunction = delayFunc
        , listOfUsersUpdate = updateListUsers
        , checkAvalible = VH.getTsAndPts vkHandle vkToken
        }

startBot ::
       Monad m
    => LogHandle m
    -> EchoBotHandle m lastUpdMark updateInf
    -> C.BotConfig
    -> m ()
startBot hLogger botHandler botConf = do
    logInfo hLogger "New Bot Start"
    logInfo hLogger "Check token"
    ch <- checkAvalible botHandler hLogger
    case ch of
        Nothing -> logError hLogger "Bad token"
        mark -> do
            logInfo hLogger "Good token"
            loopBot hLogger botHandler (HelpMessage (C.help botConf)) (mark, [])
