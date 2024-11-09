{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Id (Id (..)) where

import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics (Generic)

newtype Id = Id
    { unId :: Int
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromRow, ToRow)
    deriving newtype (Eq, Ord, FromField, ToField)
