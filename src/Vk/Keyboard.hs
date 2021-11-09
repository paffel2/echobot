module Vk.Keyboard where

import           Data.Aeson                    (encode)
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.Text                     as T
import           Vk.KeyboardJSON               (VkAction (VkAction),
                                                VkButton (VkButton),
                                                VkKeyboard (VkKeyboard))

keyboard :: T.Text
keyboard = T.pack $ BLI.unpackChars (encode keyboardVk)
  where
    keyboardVk =
        VkKeyboard
            True
            [ [VkButton (VkAction "text" "1" "1")]
            , [VkButton (VkAction "text" "2" "2")]
            , [VkButton (VkAction "text" "3" "3")]
            , [VkButton (VkAction "text" "4" "4")]
            , [VkButton (VkAction "text" "5" "5")]
            ]
