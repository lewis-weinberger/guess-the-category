{-# LANGUAGE OverloadedStrings #-}

module State where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import qualified Data.List as L
import qualified Data.Set as S
import Data.Text (Text)
import Discord.Types
import System.Random

type Category = Text

-- | The game state
data State = State
    { rng :: StdGen
    , lobby :: S.Set User
    , categories :: [Category]
    , spy :: Maybe User
    , category :: Maybe Category
    }
    deriving (Show)

-- | Create a new state from a set of categories
newState :: Int -> [Category] -> State
newState seed cats =
    State
        { rng = mkStdGen seed
        , lobby = S.empty
        , categories = cats
        , spy = Nothing
        , category = Nothing
        }

-- | Start a new round
newGame :: State -> (Text, State)
newGame state =
    if S.size (lobby state) > 2
        then
            if L.length (categories state) > 1
                then unsafeNewGame state
                else ("Not enough categories left to choose! (2+ required)", state)
        else ("Not enough players in the lobby! (3+ required)", state)

-- | Start a new round (unsafe as assumes lobby has players)
unsafeNewGame :: State -> (Text, State)
unsafeNewGame state = (msg, state')
  where
    msg = showState state
    (s, r) = uniformR (0, S.size (lobby state) - 1) (rng state)
    spy' = S.elemAt s (lobby state)
    (c, r') = uniformR (0, L.length (categories state) - 1) r
    category' = (categories state) !! c
    categories' = L.delete category' (categories state)
    state' =
        state
            { rng = r'
            , categories = categories'
            , spy = Just spy'
            , category = Just category'
            }

-- | Helper to pretty-print current round
showState :: State -> Text
showState l = L.foldl' f "Categories in this round: " (categories l)
  where
    f s x = s <> "\n- " <> x

-- | Helper to pretty-print lobby
showLobby :: S.Set User -> Text
showLobby l = S.foldl' f "Players currently in lobby: " l
  where
    f s x = s <> "\n- " <> userName x

-- | Helper to print current category
showCategory :: State -> User -> Text
showCategory s u = case (spy s, category s) of
    (Just u', Just c) -> "Category: " <> if u == u' then "???" else c
    _ -> "Game not started! Use /gtc_new to start new round"

-- | Abstraction to get current state
get :: (MonadIO m) => MVar State -> m State
get = liftIO . takeMVar

-- | Abstraction to set the current state
put :: (MonadIO m) => MVar State -> State -> m ()
put m s = liftIO $ putMVar m s
