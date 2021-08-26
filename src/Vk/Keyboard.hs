module Vk.Keyboard where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.Text as T
import Vk.KeyboardJSON
    ( VkAction(VkAction)
    , VkButton(VkButton)
    , VkKeyboard(VkKeyboard)
    )

keyboardVk :: VkKeyboard
keyboardVk = VkKeyboard True buttonsVk

b1 :: [VkButton]
b1 = [VkButton (VkAction "text" "1" "1")]

b2 :: [VkButton]
b2 = [VkButton (VkAction "text" "2" "2")]

b3 :: [VkButton]
b3 = [VkButton (VkAction "text" "3" "3")]

b4 :: [VkButton]
b4 = [VkButton (VkAction "text" "4" "4")]

b5 :: [VkButton]
b5 = [VkButton (VkAction "text" "5" "5")]

buttonsVk :: [[VkButton]]
buttonsVk = [b1, b2, b3, b4, b5]

encKeyboard :: T.Text
encKeyboard = T.pack $ BLI.unpackChars (encode keyboardVk)
