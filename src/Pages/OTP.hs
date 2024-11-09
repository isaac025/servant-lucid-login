{-# LANGUAGE ViewPatterns #-}

module Pages.OTP where

import App (tshow)
import Control.Monad (when)
import Data.Text (Text)
import Lucid
import Pages.Base (loginBase)

otpPage :: Int -> Bool -> Text -> Text -> Html ()
otpPage (tshow -> uid) isErr method url = loginBase $ do
    div_ [class_ "ui raised very padded text container segment", style_ "text-align: center; max-width: 400px;"] $ do
        h2_ [] "OTP"
        when isErr $
            div_ [class_ "ui error message"] $ do
                div_ [class_ "header"] "Bad OTP"
                p_ "The OTP entered is either expired or wrong. Please, try again."

        form_ [class_ ("ui form" <> if isErr then " error" else ""), method_ method, action_ $ url <> uid] $ do
            div_ [class_ ("field" <> if isErr then " error" else ""), style_ "text-align: left"] $ do
                label_ [for_ "otp"] "OTP"
                input_ [type_ "text", name_ "otp", id_ "otp", placeholder_ "XXXXXX", required_ "true"]
            button_ [class_ "ui positive button", type_ "submit"] "Validate"
