{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.PasswordHash (
    PasswordHash (..),
    hash,
    verifyPassword,
) where

import App (MonadIO, io)
import Crypto.KDF.BCrypt (hashPassword, validatePassword)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics (Generic)

newtype PasswordHash = PasswordHash
    { unPasswordHash :: Text
    }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, FromField, ToField)

hash :: (MonadIO m) => Text -> m ByteString
hash p = io $ hashPassword 12 (encodeUtf8 p)

verifyPassword :: Text -> PasswordHash -> Bool
verifyPassword plaintext (PasswordHash hashed) = validatePassword (encodeUtf8 hashed) (encodeUtf8 plaintext)
