{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App (
    MonadIO,
    Env (..),
    DbPool,
    AppT (..),
    runAppT,
    loadEnv,
    initPool,
    Config (..),
    asks,
    io,
    withPool,
    tshow,
) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import Data.Pool (Pool, defaultPoolConfig, newPool, withResource)
import Data.Text (Text, pack)
import Data.Yaml (decodeFileEither)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import GHC.Generics (Generic)

type DbPool = Pool Connection

data Env = Env
    { telnyxAPIKey :: Text
    , telnyxPubKey :: Text
    , telnyxNumber :: Text
    , dbConnString :: Text
    }
    deriving (Generic)

instance FromJSON Env

data Config = Config
    { env :: Env
    , dbPool :: DbPool
    }

newtype AppT a = AppT {unApp :: ReaderT Config IO a}
    deriving (Functor, Applicative, Monad, MonadReader Config, MonadIO)

runAppT :: Config -> AppT a -> IO a
runAppT config a = runReaderT (unApp a) config

loadEnv :: (MonadIO m) => m (Either String Env)
loadEnv = do
    f <- liftIO $ decodeFileEither ".env.yaml"
    case f of
        Left err -> pure $ Left ("Error loading env file: " ++ show err)
        Right val -> pure $ Right val

initPool :: ByteString -> IO DbPool
initPool creds = newPool $ defaultPoolConfig (connectPostgreSQL creds) close 10 5

io :: (MonadIO m) => IO a -> m a
io = liftIO

withPool :: (Connection -> IO b) -> AppT b
withPool f = do
    pool <- asks dbPool
    io $ withResource pool f

tshow :: (Show a) => a -> Text
tshow = pack . show
