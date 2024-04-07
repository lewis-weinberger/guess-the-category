{- | This is a Discord bot to play a simple guessing/deception game,
    based on the two examples "interaction-commands-simple.hs" and
    "state-counter.hs" from https://github.com/discord-haskell/discord-haskell
-}
module Main where

import Commands
import Control.Concurrent.MVar
import Data.List (find)
import Defaults
import Discord
import Discord.Interactions
import Discord.Types
import State
import System.Random (randomIO)
import Util

main :: IO ()
main = do
    -- Get Discord API token and server ID from environment variables
    token <- getToken
    guild <- getGuild

    -- Create our initial game state
    seed <- randomIO
    state <- newMVar (newState seed defaultCategories)

    -- Run Discord event loop
    err <-
        runDiscord $
            def
                { discordToken = token
                , discordOnEvent = onEvent guild state
                , discordGatewayIntent = def{gatewayIntentMessageContent = False}
                }

    -- If something has gone wrong, log it and finish
    logger (show err)

-- | Handle events received from Discord
onEvent :: GuildId -> MVar (State) -> Event -> DiscordHandler ()
onEvent guild state event = case event of
    Ready _ _ _ _ _ _ (PartialApplication app _) -> onReady app guild
    InteractionCreate cmd -> onInteraction state cmd
    _ -> return ()

-- | Handle "Ready" event from Discord
onReady :: ApplicationId -> GuildId -> DiscordHandler ()
onReady app guild = do
    logger "Discord ready! Registering slash commands..."

    -- Attempt to register slash commands
    cmds <- mapM (register app guild) slashCommands
    case sequence cmds of
        Right _ -> logger "Commands successfully registered"
        Left e -> logger (show e) >> stopDiscord

-- | Handle "Interaction" event from Discord
onInteraction :: MVar (State) -> Interaction -> DiscordHandler ()
onInteraction
    s
    i@InteractionApplicationCommand
        { applicationCommandData = input@ApplicationCommandDataChatInput{}
        } = do
        case find (\c -> applicationCommandDataName input == name c) slashCommands of
            Just cmd -> handler cmd i (optionsData input) s
            Nothing -> logger "Unknown slash command received"
onInteraction _ _ = return ()

-- | Slash commands to register with Discord
slashCommands :: [CommandSpec]
slashCommands =
    [ CommandSpec "join" "Join the game lobby" joinCommand Nothing
    , CommandSpec "leave" "Leave the game lobby" leaveCommand Nothing
    , CommandSpec
        "set"
        "Set categories for game"
        setCommand
        (Just $ stringOption "categories" "New categories for the game")
    , CommandSpec "new" "Start a new round" newCommand Nothing
    , CommandSpec
        "reveal"
        "Reveal assignment for this round"
        revealCommand
        Nothing
    ]
