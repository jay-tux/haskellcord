{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

import Control.Monad (when)
import Data.Text (isPrefixOf, toLower, Text, pack)
import qualified Data.Text.IO as TIO

import UnliftIO
import UnliftIO.Concurrent

import Token

import Discord
import Discord.Types
import qualified Discord.Requests as R

main :: IO ()
main = pingpongExample

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do putStrLn "Bot online!"
                     userFacingError <- runDiscord $ def
                                            { discordToken = pack token
                                            , discordOnEvent = eventHandler }
                     TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
       MessageCreate m -> when (not (fromBot m) && isPing (messageText m)) $ do
--               _ <- restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
--               threadDelay (4 * 10^6)
               _ <- restCall (R.CreateMessage (messageChannel m) "Pong!")
               pure ()
       _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: Text -> Bool
isPing = ("ping" `isPrefixOf`) . toLower
