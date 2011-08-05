module Network.Bitcoin (
    callBitcoinAPI
)
where
import Data.Maybe (fromJust)
import Network.Browser
import Network.HTTP
import Network.URI (URI,parseURI)

jsonRPC cmd params = "{\"jsonrpc\":\"2.0\",\"method\":\"" ++ cmd ++ "\", \"params\":[], \"id\":1}"
-- [ ("jsonrpc","2.0"),("method",cmd),("params",[]),("id":1) ]

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
