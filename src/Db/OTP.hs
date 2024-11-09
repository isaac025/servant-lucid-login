{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db.OTP where

import App
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple (FromRow, Only (..), ToRow, execute, query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Generics (Generic)
import Types.Id
import Types.OTP

data OTP = OTP
    { otpId :: Id
    , user_id :: Int
    , otp :: OTPCode
    , expiry :: UTCTime
    }
    deriving (Generic, Show)
    deriving anyclass (FromRow, ToRow)

class (Monad m) => MonadOTP m where
    storeOTPSession :: Id -> OTPCode -> UTCTime -> m ()
    validateOTPSession :: OTPCode -> Id -> m Bool

instance MonadOTP AppT where
    storeOTPSession uid o time = do
        withPool $ \conn -> do
            _ <- execute conn [sql| INSERT INTO otp_session (user_id, otp, expiry) VALUES (?, ?, ?)|] (uid, o, time)
            pure ()
    validateOTPSession o uid = do
        withPool $ \conn -> do
            l <- query conn [sql| SELECT id FROM otp_session WHERE otp = ? AND user_id = ?|] (o, uid)
            case l of
                [Only (x :: Id)] -> execute conn [sql| DELETE FROM otp_session WHERE id = ? |] x >> pure True
                _ -> pure False
