{- This is a little program to remove old slash commands
   previously registered by the bot.

   Usage: cabal run slash_clean

   You need to set the environment variable SLASH_PREFIX to
   the desired prefix: where this is matched the command will
   be unregistered.
-}

module Main where

import Control.Monad (forM_, void)
import Data.Text (Text, isPrefixOf, pack, unpack)
import Discord
import Discord.Interactions
import qualified Discord.Requests as R
import Discord.Types
import System.Environment (getEnv)
import Util

main :: IO ()
main = do
    -- Required inputs from environment variables
    token <- getToken
    guild <- getGuild
    prefix <- fmap pack (getEnv "SLASH_PREFIX")

    -- Run Discord event loop
    err <-
        runDiscord $
            def
                { discordToken = token
                , discordOnEvent = onEvent guild prefix
                , discordGatewayIntent = def{gatewayIntentMessageContent = False}
                }

    -- If something has gone wrong, log it and finish
    logger (show err)

-- | Dispatch events received from Discord
onEvent :: GuildId -> Text -> Event -> DiscordHandler ()
onEvent guild prefix event = case event of
    Ready _ _ _ _ _ _ (PartialApplication app _) -> onReady app guild prefix
    _ -> return ()

-- | Handle "Ready" event from Discord
onReady :: ApplicationId -> GuildId -> Text -> DiscordHandler ()
onReady app guild prefix = do
    logger "Discord ready! Un-registering slash commands..."

    -- Get registered slash commands
    registered <- restCall $ R.GetGuildApplicationCommands app guild
    case registered of
        Right cmds -> unregister cmds prefix app guild
        Left e -> logger (show e)

    logger "Stopping..."
    stopDiscord

-- | Un-register commands starting with given prefix
unregister ::
    [ApplicationCommand] -> Text -> ApplicationId -> GuildId -> DiscordHandler ()
unregister cmds prefix app guild = do
    forM_ matched $ \cmd -> do
        void . restCall $
            R.DeleteGuildApplicationCommand app guild (applicationCommandId cmd)
        logger $ (unpack $ applicationCommandName cmd) ++ " un-registered!"
  where
    matched = filter (\c -> prefix `isPrefixOf` (applicationCommandName c)) cmds
