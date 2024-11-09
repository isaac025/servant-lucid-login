{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server (runApp) where

import App
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Db
import Lucid (Html)
import Network.Wai.Handler.Warp
import Pages
import Servant
import Servant.HTML.Lucid
import Servant.Multipart
import System.Exit (exitFailure)
import Telnyx.Service
import Types

type LoginAPI = "login" :> Get '[HTML] (Html ())

type ForgotAPI =
    "forgot-password"
        :> ( Get '[HTML] (Html ())
                :<|> "submit" :> ReqBody '[FormUrlEncoded] Email :> Post '[HTML] (Html ())
                :<|> "verify-otp" :> Capture "id" Int :> ReqBody '[FormUrlEncoded] OTPCode :> Post '[HTML] (Html ())
                :<|> "reset" :> Capture "id" Int :> Get '[HTML] (Html ())
                :<|> Capture "id" Int :> "submit" :> ReqBody '[FormUrlEncoded] ResetData :> Post '[HTML] (Html ())
           )
type OtpAPI =
    "otp" :> ReqBody '[FormUrlEncoded] LoginData :> Post '[HTML] (Html ())
        :<|> "verify-otp" :> Capture "id" Int :> ReqBody '[FormUrlEncoded] OTPCode :> Post '[HTML] (Html ())

type HomeAPI =
    "home"
        :> ( Get '[HTML] (Html ())
                :<|> "upload" :> MultipartForm Mem (MultipartData Mem) :> Post '[HTML] (Html ())
           )

type FilesAPI = "static" :> Raw

type API = LoginAPI :<|> OtpAPI :<|> ForgotAPI :<|> HomeAPI :<|> FilesAPI

forgotServer :: ServerT ForgotAPI AppT
forgotServer = getForgotPassword :<|> getUserForForgotPassword :<|> forgotPasswordOTP :<|> getReset :<|> resetHandler
  where
    getForgotPassword :: AppT (Html ())
    getForgotPassword = pure (forgotPasswordPage False)

    getUserForForgotPassword :: Email -> AppT (Html ())
    getUserForForgotPassword e = do
        muser <- getUserByEmail e
        case muser of
            Nothing -> pure (forgotPasswordPage True)
            Just user -> do
                o <- sendTelnyxSms (mobile user)
                now <- io getCurrentTime
                let expiryDate = addUTCTime 300 now
                storeOTPSession (userId user) o expiryDate
                pure (otpPage (unId $ userId user) False "get" "/forgot-password/reset/")

    forgotPasswordOTP :: Int -> OTPCode -> AppT (Html ())
    forgotPasswordOTP uid o = do
        b <- validateOTPSession o (Id uid)
        if b then pure (resetPasswordPage uid False) else pure $ otpPage uid True "get" "/forgot-password/reset/"

    getReset :: Int -> AppT (Html ())
    getReset uid = pure (resetPasswordPage uid False)

    resetHandler :: Int -> ResetData -> AppT (Html ())
    resetHandler uid (ResetData p1 p2) =
        if p1 == p2
            then do
                passHash <- decodeUtf8 <$> hash p1
                updatePassword (Id uid) (PasswordHash passHash)
                pure (loginPage False)
            else pure $ resetPasswordPage uid True

server :: ServerT API AppT
server = loginHandler :<|> (submitHandler :<|> otpHandler) :<|> forgotServer :<|> (homeHandler :<|> uploadReportHandler) :<|> serveDirectoryFileServer "static"
  where
    loginHandler :: AppT (Html ())
    loginHandler = pure (loginPage False)

    submitHandler :: LoginData -> AppT (Html ())
    submitHandler l = do
        muser <- getUserByEmail (Email $ loginEmail l)
        case muser of
            Nothing -> pure (loginPage True)
            Just user -> do
                o <- sendTelnyxSms (mobile user)
                now <- io getCurrentTime
                let expiryDate = addUTCTime 300 now
                storeOTPSession (userId user) o expiryDate
                pure (otpPage (unId $ userId user) False "post" "/verify-otp/")

    otpHandler :: Int -> OTPCode -> AppT (Html ())
    otpHandler uid o = do
        b <- validateOTPSession o (Id uid)
        if b then pure homePage else pure $ otpPage uid True "post" "/verify-otp/"

    homeHandler :: AppT (Html ())
    homeHandler = pure homePage

    uploadReportHandler :: MultipartData Mem -> AppT (Html ())
    uploadReportHandler multipart =
        case lookupFile "file-upload" multipart of
            Left _ -> pure homePage
            Right file -> do
                let fContent = decodeUtf8 $ toStrict (fdPayload file)
                io $ print fContent
                pure homePage

api :: Proxy API
api = Proxy

appToHandler :: Config -> AppT a -> Handler a
appToHandler config a = io $ runAppT config a

serverApp :: Config -> Server API
serverApp config = hoistServer api (appToHandler config) server

app :: Config -> Application
app config = serve api (serverApp config)

runApp :: IO ()
runApp = do
    menv <- loadEnv
    case menv of
        Left err -> putStrLn err >> exitFailure
        Right e -> do
            pool <- initPool (encodeUtf8 (dbConnString e))
            run 8080 (app (Config e pool))
