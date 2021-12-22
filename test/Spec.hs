import           EchoTests       (echoTests)

import           UsersListsTests (usersListsTests)

main :: IO ()
main = do
    echoTests
    usersListsTests
