module Util where

import Control.Monad.IO.Class
import Data.Text (Text, pack)
import Discord.Types
import System.Environment (getEnv)

-- | Get Discord API token environment variable
getToken :: IO Text
getToken = fmap pack (getEnv "DISCORD_TOKEN")

-- | Get Discord Guild ID from environment variable
getGuild :: IO GuildId
getGuild = fmap read (getEnv "DISCORD_GUILD")

-- | Log text to STDOUT
logger :: (MonadIO m) => String -> m ()
logger = liftIO . putStrLn
