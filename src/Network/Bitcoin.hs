{-# LANGUAGE OverloadedStrings,DeriveDataTypeable #-}
-- | Communicate with a Bitcoin daemon over JSON RPC
module Network.Bitcoin
    (
        -- * Types
      BitcoinAuth(..)
    , BitcoinAmount
    , BitcoinException(..)

    -- * Individual API methods
    , getBalance

    -- * Low-level API
    , callBitcoinApi
    ) where
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Attoparsec
import Data.Attoparsec.Number
import Data.Fixed
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Typeable
import Network.Browser
import Network.HTTP hiding (password)
import Network.URI (parseURI)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M


-- Define Bitcoin's internal precision
data Satoshi = Satoshi
instance HasResolution Satoshi where
    resolution _ = 10^(8::Integer)

-- | Fixed precision Bitcoin arithmetic (to avoid floating point errors)
type BitcoinAmount = Fixed Satoshi

-- | Name of a Bitcoin wallet account
type AccountName = String

-- | Minimum number of confirmations for a payment
type MinConf     = Integer

-- | 'BitcoinAuth' describes authentication credentials for
-- making API requests to the Bitcoin daemon
data BitcoinAuth = BitcoinAuth
    { rpcUrl :: String      -- ^ URL, with port, where bitcoind listens
    , rpcUser :: String     -- ^ same as bitcoind's 'rpcuser' config
    , rpcPassword :: String -- ^ same as bitcoind's 'rpcpassword' config
    }
    deriving (Show)

data BitcoinRpcResponse = BitcoinRpcResponse {
        btcResult  :: Value,
        btcError   :: Value
    }
    deriving (Show)
instance FromJSON BitcoinRpcResponse where
    parseJSON (Object v) = BitcoinRpcResponse <$> v .: "result"
                                          <*> v .: "error"
    parseJSON _ = mzero

-- |A 'BitcoinException' is thrown when 'callBitcoinApi' encounters an
-- error.  The API error code is represented as an @Int@, the message as
-- a @String@.
data BitcoinException
    = BitcoinApiError Int String
    deriving (Show,Typeable)
instance Exception BitcoinException

-- encodes an RPC request into a ByteString containing JSON
jsonRpcReqBody :: String -> [Value] -> BL.ByteString
jsonRpcReqBody cmd params = encode $ object [
                "jsonrpc" .= ("2.0"::String),
                "method"  .= cmd,
                "params"  .= params,
                "id"      .= (1::Int)
              ]

-- |'callBitcoinApi' is a low-level interface for making authenticated API
-- calls to a Bitcoin daemon.  The first argument specifies
-- authentication details (URL, username, password) and is often
-- curried for convenience:
--
-- > callBtc = callBitcoinApi $ BitcoinAuth "http://127.0.0.1:8332" "user" "password"
--
-- The second argument is the command name.  The third argument provides
-- parameters for the API call.
--
-- > let result = callBtc "getbalance" ["account-name", Number 6]
--
-- On error, throws a 'BitcoinException'
callBitcoinApi :: BitcoinAuth  -- ^ authentication credentials for bitcoind
               -> String  -- ^ command name
               -> [Value] -- ^ command arguments
               -> IO Value
callBitcoinApi auth command params = do
    (_,httpRes) <- browse $ do
        setOutHandler $ const $ return ()
        addAuthority authority
        setAllowBasicAuth True
        request $ httpRequest urlString $ jsonRpcReqBody command params
    let res = fromSuccess $ fromJSON $ toValue $ rspBody httpRes
    case res of
        BitcoinRpcResponse {btcError=Null} -> return $ btcResult res
        BitcoinRpcResponse {btcError=e}    -> throw $ buildBtcError e
    where authority     = httpAuthority auth
          urlString     = rpcUrl auth
          toStrict      = B.concat . BL.toChunks
          justParseJSON = fromJust . maybeResult . parse json
          toValue       = justParseJSON . toStrict

-- Internal helper functions to make callBitcoinApi more readable
httpAuthority :: BitcoinAuth -> Authority
httpAuthority (BitcoinAuth urlString username password) =
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

fromSuccess :: Data.Aeson.Result t -> t
fromSuccess (Success a) = a
fromSuccess (Error   s) = error s

buildBtcError :: Value -> BitcoinException
buildBtcError (Object o) = BitcoinApiError code msg
    where find k = fromSuccess . fromJSON . fromJust . M.lookup k
          code = find "code" o
          msg  = find "message" o
buildBtcError _ = error "Need an object to buildBtcError"

-- Minimize rounding troubles when converting Attoparsec numbers
-- into Bitcoin amounts
n2btc :: Number -> BitcoinAmount
n2btc (I i) = fromInteger i
n2btc (D d) = fromRational $ toRational d

-- | Returns the balance of a specific Bitcoin account
getBalance :: BitcoinAuth
           -> AccountName
           -> MinConf
           -> IO BitcoinAmount
getBalance auth acct minconf = do
    (Number balance) <- callBitcoinApi auth "getbalance" args
    return $ n2btc balance
  where
    args = [ String $ fromString acct, Number $ fromInteger minconf ]
