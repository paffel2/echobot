{-# LANGUAGE NamedFieldPuns #-}

module Vk.Impl where

import qualified Echo         as E
import           UsersLists   (ChatId (getChatId))
import           Vk.Responses (VkItem (VkItem, vkItemAttachments, vkItemFromId, vkItemGeo, vkItemPayload, vkItemText),
                               VkMessageTypes (..))

fromItemToUsersMessages :: VkItem -> Maybe (E.UserMessage VkMessageTypes)
fromItemToUsersMessages VkItem {vkItemText = "/help", vkItemFromId = chatId} =
    if getChatId chatId > 0
        then Just $ E.UserMessage chatId (E.CommandMessage E.Help)
        else Nothing
fromItemToUsersMessages VkItem {vkItemText = "/repeat", vkItemFromId = chatId} =
    if getChatId chatId > 0
        then Just $ E.UserMessage chatId (E.CommandMessage E.ChoicesRequest)
        else Nothing
fromItemToUsersMessages VkItem {vkItemFromId = chatId, vkItemPayload = (Just n)} =
    if getChatId chatId > 0
        then Just $ E.UserMessage chatId (E.CommandMessage (E.Repeat n))
        else Nothing
fromItemToUsersMessages message@VkItem { vkItemAttachments = []
                                       , vkItemFromId = chatId
                                       } =
    if getChatId chatId > 0
        then Just $
             E.UserMessage
                 chatId
                 (E.JustMessage (VkTextMessage $ vkItemText message))
        else Nothing
fromItemToUsersMessages VkItem {vkItemFromId = chatId, vkItemGeo = Just geo} =
    if getChatId chatId > 0
        then Just $ E.UserMessage chatId (E.JustMessage (VkGeoMessage geo))
        else Nothing
fromItemToUsersMessages VkItem { vkItemAttachments = something
                               , vkItemFromId = chatId
                               , vkItemGeo = Nothing
                               } =
    if getChatId chatId > 0
        then Just $
             E.UserMessage
                 chatId
                 (E.JustMessage $ VkWithAttachmentsMessage something)
        else Nothing
