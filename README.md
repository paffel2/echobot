# echobot

This repository contains a echo bot for Telegram and VKontakte. The bot sends messages from the user to him in response. The project was created as a test task of the Metalamp company.
 
# Installation and Startup Guide
To run and install the program, you must install the [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).
After installing stack, clone this repository. Install the compiler (if required) and build the project.

    # install compiler
      stack setup
    # build project
      stack build
## Configuration file
        bot {
            type = "VKBot"/"TelegramBot"
            token = "token"
            help = "help message"
            }
        logger {
            priority = "Debug"/"Info"/"Warning"/"Error"
               }


To the run bot use 

    stack exec echobot-exe

If the help message contains cyrillic characters, use (for Windows)

    chcp 65001; stack exec echobot-exe
    
# Project structure

* `app/Main.hs` - Preparing the bot settings and launch.
* `src/Telegram` - contains all code used by telegram bot.
    * `Telegram/API.hs` - contains functions that implement Telegram API.
    * `Telegram/Bot.hs` - telegram bot launcher
    * `Telegram/BuildRequests.hs` - contains the functions for creating requests to the telegram server.
    * `Telegram/Echo.hs` - contains echo reply functions.
    * `Telegram/Keyboard.hs` - contains the keyboard implementation.
    * `Telegram/Requests.hs` - contains data models received during requests to the telegram server.
    * `Telegram/Responses.hs` - contains models of responses sent to the telegram server.
    * `Telegram/TelegramHandle.hs` - contains a handle using all necessary functions.
* `src/VK` - contains all code used by VKontakte bot.
    * `VK/API.hs` - contains functions that implement VKontakte API.
    * `VK/Bot.hs` - VKontakte bot launcher
    * `VK/BuildRequests.hs` - contains the functions for creating requests to the VKontakte server.
    * `VK/Echo.hs` - contains echo reply functions.
    * `VK/Keyboard.hs` - contains the keyboard implementation.
    * `VK/KeyboardJSON.hs` - contains keyboard data models received during requests to the VKontakte server.
    * `VK/Responses.hs` - contains models of responses sent to the VKontakte server.
    * `VK/VKHandle.hs` - contains a handle using all necessary functions.
* `src/Config.hs` - contains operations related to bot settings.
* `src/Logger.hs` - contains the logger implementation.
* `test` - contains tests.
* `config/bot.template.conf` - bot's config file. Сontains a configuration file with a description of the parameters 


test2

test


