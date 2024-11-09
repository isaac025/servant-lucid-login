{-# LANGUAGE ViewPatterns #-}

module Pages.Login where

import App (tshow)
import Control.Monad (when)
import Lucid
import Pages.Base (loginBase)

loginPage :: Bool -> Html ()
loginPage isErr = loginBase $ do
    div_ [class_ "ui raised very padded text container segment", style_ "text-align: center; max-width: 400px;"] $ do
        h2_ [] "Login"
        when isErr $
            div_ [class_ "ui error message"] $ do
                div_ [class_ "header"] "Invalid session"
                p_ "The email and/or password is incorrect. Please, try again."

        form_ [class_ ("ui form" <> if isErr then " error" else ""), method_ "post", action_ "/otp"] $ do
            div_ [class_ ("field" <> if isErr then " error" else ""), style_ "text-align: left"] $ do
                label_ [for_ "email"] "Email"
                input_ [type_ "email", name_ "email", id_ "email", placeholder_ "Enter your email", required_ "true"]
            div_ [class_ ("field" <> if isErr then " error" else ""), style_ "text-align: left"] $ do
                label_ [for_ "password"] "Password"
                input_ [type_ "password", name_ "password", id_ "password", placeholder_ "Enter your password", required_ "true"]
            div_ [style_ "text-align: right"] $ do
                a_ [href_ "http://localhost:8080/forgot-password"] "Forgot password?"
            button_ [class_ "ui positive button", type_ "submit"] "Login"

forgotPasswordPage :: Bool -> Html ()
forgotPasswordPage isErr = loginBase $ do
    div_ [class_ "ui raised very padded text container segment", style_ "text-align: center; max-width: 400px;"] $ do
        h2_ [] "Forgot Password"
        when isErr $
            div_ [class_ "ui error message"] $ do
                div_ [class_ "header"] "Email does not exist!"
                p_ "The email entered does not exist. Pease, try again."
        form_ [class_ ("ui form" <> if isErr then " error" else ""), method_ "post", action_ "/forgot-password/submit"] $ do
            div_ [class_ ("field" <> if isErr then " error" else ""), style_ "text-align: left"] $ do
                label_ [for_ "email"] "Please enter your email"
                input_ [type_ "email", name_ "email", id_ "email", placeholder_ "Email", required_ "true"]
            button_ [class_ "ui positive button", type_ "submit"] "Continue"

resetPasswordPage :: Int -> Bool -> Html ()
resetPasswordPage (tshow -> uid) isErr = loginBase $ do
    div_ [class_ "ui raised very padded text container segment", style_ "text-align: center; max-width: 400px;"] $ do
        h2_ [] "Reset Password"
        when isErr $
            div_ [class_ "ui error message"] $ do
                div_ [class_ "header"] "Passwords do not match!"
                p_ "The passwords entered do not match. Please, try again."
        form_ [class_ ("ui form" <> if isErr then " error" else ""), method_ "post", action_ $ "/forgot-password/" <> uid <> "/submit"] $ do
            div_ [class_ ("field" <> if isErr then " error" else ""), style_ "text-align: left"] $ do
                label_ [for_ "password"] "New Password"
                input_ [type_ "password", name_ "password", id_ "password", placeholder_ "Password", required_ "true"]
            div_ [class_ ("field" <> if isErr then " error" else ""), style_ "text-align: left"] $ do
                label_ [for_ "confirm-password"] "Confirm Password"
                input_ [type_ "password", name_ "confirm-password", id_ "confirm-password", placeholder_ "Confirm Password", required_ "true"]
            button_ [class_ "ui positive button", type_ "submit"] "Confirm"
