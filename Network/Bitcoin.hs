{-# LANGUAGE OverloadedStrings #-}
module Network.Bitcoin (
    callBitcoinAPI
)
where
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Attoparsec
import Data.Maybe (fromJust)
import Network.Browser
import Network.HTTP
import Network.URI (URI,parseURI)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

data BtcRpcResponse = BtcRpcResponse {
        btcResult  :: Value,
        btcError   :: Value
    }
    deriving (Show)
instance FromJSON BtcRpcResponse where
    parseJSON (Object v) = BtcRpcResponse <$> v .: "result"
                                          <*> v .: "error"
    parseJSON _ = mzero

-- encodes an RPC request into a ByteString containing JSON
jsonRpcReqBody :: String -> [String] -> BL.ByteString
jsonRpcReqBody cmd params = encode $ object [
                "jsonrpc" .= ("2.0"::String),
                "method"  .= cmd,
                "params"  .= params,
                "id"      .= (1::Int)
              ]

callBitcoinAPI :: String -> String -> String -> String -> [String] -> IO Value
callBitcoinAPI urlString username password command params = do
    (_,httpRes) <- browse $ do
        addAuthority authority
        setAllowBasicAuth True
        request $ httpRequest urlString $ jsonRpcReqBody command params
    -- TODO handle a failed request (what if the daemon is gone?)
    let res = fromSuccess $ fromJSON $ toValue $ rspBody httpRes
    case res of
        BtcRpcResponse {btcError=Null} -> return $ btcResult res
        -- TODO throw a BitcoinError exception if there's an error
        BtcRpcResponse {btcError=e}    -> error "TODO make an Exception"
    where authority     = btcAuthority urlString username password
          toStrict      = B.concat . BL.toChunks
          justParseJSON = fromJust . maybeResult . parse json
          toValue       = justParseJSON . toStrict
          fromSuccess x =
            case x of
                Success a -> a
                Error s   -> error s


-- Internal helper functions to make callBitcoinAPI more readable
btcAuthority :: String -> String -> String -> Authority
btcAuthority urlString username password =
    AuthBasic {
        auRealm    = "jsonrpc",
        auUsername = username,
        auPassword = password,
        auSite     = uri
    }
    where uri = fromJust $ parseURI urlString
httpRequest :: String -> BL.ByteString -> Request BL.ByteString
httpRequest urlString jsonBody =
    (postRequest urlString){
        rqBody = jsonBody,
        rqHeaders = [
            mkHeader HdrContentType "application/json",
            mkHeader HdrContentLength (show $ BL.length jsonBody)
        ]
    }
