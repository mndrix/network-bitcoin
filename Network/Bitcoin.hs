{-# LANGUAGE OverloadedStrings #-}
module Network.Bitcoin (
    callBitcoinAPI
)
where
import Data.Aeson
import Data.Maybe (fromJust)
import Network.Browser
import Network.HTTP
import Network.URI (URI,parseURI)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T

jsonRPC :: String -> [String] -> String
jsonRPC cmd params = B.unpack $ encode $ object [
                "jsonrpc" .= ("2.0"::String),
                "method"  .= cmd,
                "params"  .= params,
                "id"      .= (1::Int)
              ]

callBitcoinAPI :: String -> String -> String -> String -> [String] -> IO String
callBitcoinAPI urlString username password command params = do
    (_,res) <- browse $ do
        addAuthority authority
        setAllowBasicAuth True
        -- TODO "error" is null if no errors
        -- TODO "error" is hashref if errors (see "code" and "message")
        -- TODO "result" has data
        request $ buildRequest urlString $ jsonRPC command params
    return $ rspBody res
    where authority = buildAuthority urlString username password



-- Internal helper functions to make callBitcoinAPI more readable
buildAuthority :: String -> String -> String -> Authority
buildAuthority urlString username password =
    AuthBasic {
        auRealm    = "jsonrpc",
        auUsername = username,
        auPassword = password,
        auSite     = uri
    }
    where uri = fromJust $ parseURI urlString
buildRequest :: String -> String -> Request String
buildRequest urlString jsonBody =
    (postRequest urlString){
        rqBody = jsonBody,
        rqHeaders = [
            mkHeader HdrContentType "application/json",
            mkHeader HdrContentLength (show $ length jsonBody)
        ]
    }
