{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}

module Db.User where

import App
import Control.Monad (void)
import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow, ToRow, execute, query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Generics (Generic)
import Types.Email
import Types.Id
import Types.PasswordHash
import Types.Username

data User = User
    { userId :: Id
    , username :: Username
    , email :: Email
    , password :: PasswordHash
    , mobile :: Text
    }
    deriving (Generic)
    deriving anyclass (FromRow, ToRow)

class (Monad m) => MonadUsers m where
    getUserByEmail :: Email -> m (Maybe User)
    updatePassword :: Id -> PasswordHash -> m ()

instance MonadUsers AppT where
    getUserByEmail t = do
        withPool $ \conn -> do
            ls <- query conn [sql| SELECT * FROM users WHERE email = ?|] t
            if null ls then pure Nothing else pure $ Just (head ls)
    updatePassword uid pwd = do
        withPool $ \conn -> do
            void $ execute conn [sql| UPDATE users SET password = ? WHERE user_id = ? |] (pwd, uid)
