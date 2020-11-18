module Modules.Commands (
    commands, isCommand, extractCommand, prefix, runCommand
) where

import Data.Text (isPrefixOf, toLower, Text, pack, unpack)
import Modules.Utils
import qualified Modules.Commands.AllCommands as AC

-- ==== Exported methods ====
-- | Gets the current Prefix
prefix :: Char
prefix = '~'

-- | Gets all existing commands.
commands :: Dict String (AC.Command)
commands = Dict [ ("ping", AC.ping) ]

-- | Tests whether a Text contains a command.
isCommand :: Text -> Bool
isCommand = isCommandDriver . unpack

-- | Extracts the command word from a Text, and returns it as Text.
extractCommand :: Text -> Text
extractCommand = wrapper extractCommandDriver

-- | Runs a command, or a failure method, returns a Text.
runCommand :: Text -> Text
runCommand = wrapper runCommandDriver


--- ==== Private methods ====
wrapper :: (String -> String) -> Text -> Text
wrapper f = pack . f . unpack

isCommandDriver :: String -> Bool
isCommandDriver []   = False        --foolproof
isCommandDriver cmnd = and [ prefix == head cmnd, commands `containsKey` (extractCommandDriver cmnd)  ]

extractCommandDriver :: String -> String
extractCommandDriver []        = []
extractCommandDriver (pr:comm) = head $ words comm

extractArgs :: String -> String
extractArgs []        = []
extractArgs (pr:comm) = unwords $ tail $ words comm

runCommandDriver :: String -> String
runCommandDriver c = (AC.func $ extractOrElse commands (extractCommandDriver c) (errorMessage c)) $ extractArgs c

errorMessage :: String -> AC.Command
errorMessage cmd = AC.Command "" (\_ -> "Command ``" ++ cmd ++ "`` doesn't exist. Use the command ``help`` for all commands.")
