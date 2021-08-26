module Vk.KeyboardJSON where

import Data.Aeson
    ( Options(fieldLabelModifier, omitNothingFields)
    , ToJSON(toJSON)
    , camelTo2
    , defaultOptions
    , genericToJSON
    )
import GHC.Generics (Generic)

data VkKeyboard =
    VkKeyboard
        { vkKeyboardOneTime :: Bool
        , vkKeyboardButtons :: [[VkButton]]
        }
    deriving (Show, Generic)

instance ToJSON VkKeyboard where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop 10
                , omitNothingFields = True
                }

newtype VkButton =
    VkButton
        { vkButtonAction :: VkAction
        }
    deriving (Show, Generic)

instance ToJSON VkButton where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop 8
                , omitNothingFields = True
                }

data VkAction =
    VkAction
        { vkActionType :: String
        , vkActionLabel :: String
        , vkActionPayload :: String
        }
    deriving (Show, Generic)

instance ToJSON VkAction where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop 8
                , omitNothingFields = True
                }
