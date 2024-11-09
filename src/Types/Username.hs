{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Username (Username (..)) where

import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics (Generic)

newtype Username = Username
    { unUsername :: Text
    }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, FromField, ToField)
