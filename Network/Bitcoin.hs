module Network.Bitcoin (
    callBitcoinAPI
)
where
import Data.Maybe (fromJust)
import Network.Browser
import Network.HTTP
import Network.URI (URI,parseURI)

jsonRPC cmd = "{\"jsonrpc\":\"2.0\",\"method\":\"" ++ cmd ++ "\", \"params\":[], \"id\":1}"

callBitcoinAPI :: String -> String -> String -> String -> [String] -> IO String
callBitcoinAPI endpoint username password command params = do
    (_,res) <- browse $ do
        addAuthority authority
        setAllowBasicAuth True
        -- TODO "error" is null if no errors
        -- TODO "error" is hashref if errors (see "code" and "message")
        -- TODO "result" has data
        request $ jsonReq $ jsonRPC command
    return $ rspBody res
    where authority = AuthBasic {
                        auRealm    = "jsonrpc",
                        auUsername = username,
                        auPassword = password,
                        auSite     = serverURI
                      }
          serverURI = fromJust $ parseURI endpoint
          jsonReq jsonBody = ( postRequest endpoint ){
                rqBody = jsonBody,
                rqHeaders = [
                    mkHeader HdrContentType "application/json",
                    mkHeader HdrContentLength (show $ length jsonBody)
                ]
            }

