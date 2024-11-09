module Telnyx.Service (sendTelnyxSms) where

import App
import Crypto.Random (getRandomBytes)
import Data.ByteString (ByteString, foldl')
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Simple
import Types (OTPCode (..), Sms (..))

telnyxEndpoint :: String
telnyxEndpoint = "https://api.telnyx.com/v2/messages"

generateCode :: IO Text
generateCode = do
    bytes <- getRandomBytes 4 :: IO ByteString
    let codeInt = ((abs . (`mod` 900000) . fromIntegral . foldl' (\acc b -> acc * 256 + fromEnum b) 0) bytes + 100000) :: Int
    pure (tshow codeInt)

mkTelnyxRequest :: Text -> AppT (Request, Text)
mkTelnyxRequest phone = do
    key <- telnyxAPIKey <$> asks env
    f <- telnyxNumber <$> asks env
    initReq <- io $ parseRequest ("POST " <> telnyxEndpoint)
    code <- io generateCode
    let reqHeaders = setRequestHeaders [("Content-Type", "application/json"), ("Accept", "application/json"), ("Authorization", "Bearer " <> encodeUtf8 key)] initReq
        body = Sms f phone ("your otp code is: " <> code) []
        finalReq = setRequestBodyJSON body reqHeaders
    pure (finalReq, code)

sendTelnyxSms :: Text -> AppT OTPCode
sendTelnyxSms phone = do
    (req, o) <- mkTelnyxRequest phone
    code <- getResponseStatusCode <$> httpNoBody req
    case code of
        200 -> pure (OTPCode o)
        _ -> io $ fail "failed to send sms"
