module Modules.Commands.AllCommands (
    Command (..), ping
) where

-- | Command type: consists of a description (String) and a function (String -> String)
data Command = Command { desc :: String, func :: (String -> String) }

ping :: Command
ping = Command "Test if the bot is listening." $ const "Pong!"
