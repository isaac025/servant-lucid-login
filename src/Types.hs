module Types (
    Sms (..),
    LoginData (..),
    ResetData (..),
    module Types.Email,
    module Types.Username,
    module Types.PasswordHash,
    module Types.OTP,
    module Types.Id,
) where

import Data.Aeson (ToJSON (toJSON), object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Email
import Types.Id
import Types.OTP
import Types.PasswordHash
import Types.Username
import Web.FormUrlEncoded (FromForm (..), parseUnique)

data Sms = Sms
    { from :: Text
    , to :: Text
    , text :: Text
    , mediaUrls :: [Text]
    }
    deriving (Generic)

instance ToJSON Sms where
    toJSON (Sms f t txt urls) =
        object
            [ "from" .= f
            , "to" .= t
            , "text" .= txt
            , "media_urls" .= urls
            ]

data LoginData = LoginData
    { loginEmail :: Text
    , loginPassword :: Text
    }
    deriving (Generic)

instance FromForm LoginData where
    fromForm f =
        LoginData
            <$> parseUnique "email" f
            <*> parseUnique "password" f

data ResetData = ResetData
    { newPassword :: Text
    , confirmPassword :: Text
    }
    deriving (Generic)

instance FromForm ResetData where
    fromForm f =
        ResetData
            <$> parseUnique "password" f
            <*> parseUnique "confirm-password" f
