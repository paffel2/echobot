import TelegramPatternTests
import UsersListsTests (usersListsTests)
import VkPatternTests (echoVkTests)

main :: IO ()
main = do
    usersListsTests
    echoVkTests
    --echoTelegramTests
