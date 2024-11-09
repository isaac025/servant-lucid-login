{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.OTP (OTPCode (..)) where

import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics (Generic)
import Web.FormUrlEncoded (FromForm (..), parseUnique)

newtype OTPCode = OTPCode
    { unOTPCode :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromRow, ToRow)
    deriving newtype (Eq, Ord, FromField, ToField)

instance FromForm OTPCode where
    fromForm f = OTPCode <$> parseUnique "otp" f
