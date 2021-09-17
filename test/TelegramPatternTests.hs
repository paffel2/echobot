module TelegramPatternTests where

import Data.Functor.Identity (Identity)
import qualified Data.Text.IO as TIO
import Logger (Handle(..), Priority(Debug))
import Telegram.Bot
import Telegram.Echo (echo)
import Telegram.TelegramHandle (TelegramHandle(..))
import Test.Hspec (describe, hspec, it, shouldBe)

logHandle :: Handle Identity
logHandle = Handle {priority = Debug, Logger.log = \prior message -> return ()}

telegramHandle :: TelegramHandle Identity
telegramHandle =
    TelegramHandle
        { getMe = \logHandle token -> return Nothing
        , sendKeyboard = \logHandle token chatId -> return Nothing
        , getUpdates = \logHandle token updateId -> return Nothing
        , getLastUpdateId = \logHandle updates -> return Nothing
        , sendTextMessage =
              \logHandle token chatId text entities -> return Nothing
        , sendAnimationMessage =
              \logHandle token chatId animation caption -> return Nothing
        , sendAudioMessage =
              \logHandle token chatId audio caption -> return Nothing
        , sendVideoMessage =
              \logHandle token chatId video caption -> return Nothing
        , sendDocumentMessage =
              \logHandle token chatId document caption -> return Nothing
        , sendPhotoMessage =
              \logHandle token chatId photo caption -> return Nothing
        , sendStickerMessage = \logHandle token chatId sticker -> return Nothing
        , sendVideoNoteMessage =
              \logHandle token chatId videoNote -> return Nothing
        , sendVoiceMessage =
              \logHandle token chatId voice caption -> return Nothing
        , sendContactMessage = \logHandle token chatId contact -> return Nothing
        , sendLocationMessage =
              \logHandle token chatId location -> return Nothing
        , sendVenueMessage = \logHandle token chatId venue -> return Nothing
        }
