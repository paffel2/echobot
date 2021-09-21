module Vk.Types where

newtype VkToken = VkToken {vk_token :: String}
newtype HelpMessage = HelpMessage {help_mess :: String}
--type UserId = Int
newtype Ts = Ts {ts' :: Int} deriving (Eq,Show)
newtype Pts = Pts {pts' :: Int} deriving (Eq,Show)
