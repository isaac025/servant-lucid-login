{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Email (Email (..)) where

import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics (Generic)
import Web.FormUrlEncoded (FromForm (..), parseUnique)

newtype Email = Email
    { unEmail :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromRow, ToRow)
    deriving newtype (Eq, Ord, FromField, ToField)

instance FromForm Email where
    fromForm f = Email <$> parseUnique "email" f
