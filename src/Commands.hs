{-# LANGUAGE OverloadedStrings #-}

module Commands (
    SlashCommand,
    CommandSpec (..),
    stringOption,
    register,
    joinCommand,
    leaveCommand,
    setCommand,
    newCommand,
    revealCommand,
) where

import Control.Concurrent.MVar
import Control.Monad (void)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Discord
import Discord.Interactions
import qualified Discord.Requests as R
import Discord.Types
import State
import Util

-- | Type synonym for Interaction handlers
type SlashCommand =
    Interaction -> Maybe OptionsData -> MVar (State) -> DiscordHandler ()

-- | Specification for registering slash commands
data CommandSpec = CommandSpec
    { name :: Text
    , description :: Text
    , handler :: SlashCommand
    , options :: Maybe Options
    }

-- | Helper to generate options spec
stringOption :: Text -> Text -> Options
stringOption n d = OptionsValues [s]
  where
    s = OptionValueString n Nothing d Nothing True (Left False) (Just 1) Nothing

-- | Register a slash command with Discord server
register ::
    ApplicationId ->
    GuildId ->
    CommandSpec ->
    DiscordHandler (Either RestCallErrorCode ApplicationCommand)
register i g CommandSpec{name = c, description = d, options = o} = do
    restCall $ R.CreateGuildApplicationCommand i g x
  where
    x = CreateApplicationCommandChatInput c Nothing d Nothing o Nothing Nothing

-- | Helper to respond to interactions
respond :: Interaction -> Text -> Bool -> DiscordHandler ()
respond i m e =
    void . restCall $ R.CreateInteractionResponse intId token msg
  where
    intId = interactionId i
    token = interactionToken i
    msg = if e then ephemeralResponse m else interactionResponseBasic m

-- | Helper to create an ephemeral response
ephemeralResponse :: Text -> InteractionResponse
ephemeralResponse t = InteractionResponseChannelMessage (r t)
  where
    r x =
        InteractionResponseMessage
            Nothing
            (Just x)
            Nothing
            Nothing
            (Just f)
            Nothing
            Nothing
    -- the discord-haskell library has a typo in the following...
    f = InteractionResponseMessageFlags [InteractionResponseMessageFlagEphermeral]

-- | Helper to pattern match User from a MemberOrUser
getUser :: MemberOrUser -> Maybe User
getUser m = case m of
    MemberOrUser (Left (GuildMember{memberUser = Just user})) -> Just user
    MemberOrUser (Right user) -> Just user
    _ -> Nothing

-- | Helper to run action following Interaction
withUser ::
    Interaction ->
    MVar (State) ->
    (User -> State -> DiscordHandler ()) ->
    DiscordHandler ()
withUser i s f = do
    case getUser (interactionUser i) of
        Just user -> do
            state <- get s
            f user state
        _ -> return ()

-- | Helper to handle lobby
manageLobby ::
    Interaction ->
    MVar (State) ->
    (User -> S.Set User -> S.Set User) ->
    DiscordHandler ()
manageLobby i s f = withUser i s $ \user state -> do
    let state' = state{lobby = f user (lobby state)}
        msg = showLobby (lobby state')
    put s state'
    respond i msg False
    logger . T.unpack $ msg

-- | Handle the join command
joinCommand :: SlashCommand
joinCommand i _ s = manageLobby i s S.insert

-- | Handle the leave command
leaveCommand :: SlashCommand
leaveCommand i _ s = manageLobby i s S.delete

-- | Handle the set command
setCommand :: SlashCommand
setCommand
    i
    ( Just
            (OptionsDataValues [OptionDataValueString{optionDataValueString = Right t}])
        )
    s = do
        state <- get s
        let categories' = T.splitOn "," t
            state' = state{categories = categories'}
            msg = showState state'
        put s state'
        respond i msg False
        logger . T.unpack $ msg
setCommand i _ _ = respond i "Set command requires argument!" False

-- | Handle the new command
newCommand :: SlashCommand
newCommand i _ s = do
    state <- get s
    let (msg, state') = newGame state
    put s state'
    respond i msg False
    logger . T.unpack $ msg

-- | Handle the reveal command
revealCommand :: SlashCommand
revealCommand i _ s = withUser i s $ \user state -> do
    let msg = showCategory state user
    put s state
    respond i msg True
