{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Communicate with a Bitcoin daemon over JSON RPC
module Network.Bitcoin
    (
    -- * Types
      BitcoinAuth(..)
    , Address
    , mkAddress
    , AccountName
    , MinConf
    , AddressValidation
    , isValid
    , isMine
    , account
    , BitcoinAmount
    , BitcoinException(..)

    -- * Individual API methods
    , getBalance
    , getBlockCount
    , getBlockNumber
    , getConnectionCount
    , getDifficulty
    , getGenerate
    , getHashesPerSec
    , getReceivedByAccount
    , getReceivedByAddress
    , isValidAddress
    , validateAddress

    -- * Low-level API
    , callBitcoinApi
    ) where
import Network.Bitcoin.Address

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
import qualified Data.Text as T


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
    let res = fromSuccess $ fromJSON $ toVal $ rspBody httpRes
    case res of
        BitcoinRpcResponse {btcError=Null} -> return $ btcResult res
        BitcoinRpcResponse {btcError=e}    -> throw $ buildBtcError e
    where authority     = httpAuthority auth
          urlString     = rpcUrl auth
          toStrict      = B.concat . BL.toChunks
          justParseJSON = fromJust . maybeResult . parse json
          toVal         = justParseJSON . toStrict

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

-- Convert JSON numeric values to more specific numeric types
class FromNumber a where
    fromNumber :: Number -> a
instance FromNumber BitcoinAmount where
    fromNumber (I i) = fromInteger i
    fromNumber (D d) = fromRational $ toRational d
instance FromNumber Integer where
    fromNumber (I i) = i
    fromNumber (D d) = round d
instance FromNumber Double where
    fromNumber (I i) = fromInteger i
    fromNumber (D d) = d

-- Class of types that can be converted to a JSON representation
class ToValue a where
    toValue :: a -> Value
instance ToValue Address where
    toValue addr = String $ fromString $ show addr
instance ToValue MinConf where
    toValue conf = Number $ fromInteger conf
instance ToValue AccountName where
    toValue acct = String $ fromString acct

callNumber :: FromNumber a => String -> [Value] -> BitcoinAuth -> IO a
callNumber cmd args auth = do
    (Number n) <- callBitcoinApi auth cmd args
    return $ fromNumber n

callBool :: String -> [Value] -> BitcoinAuth -> IO Bool
callBool cmd args auth = do
    (Bool b) <- callBitcoinApi auth cmd args
    return b

-- | Returns the balance of a specific Bitcoin account
getBalance :: BitcoinAuth
           -> AccountName
           -> MinConf
           -> IO BitcoinAmount
getBalance auth acct minconf = callNumber "getbalance" args auth
  where
    args = [ String $ fromString acct, Number $ fromInteger minconf ]

-- | Returns the number of blocks in the longest block chain
getBlockCount :: BitcoinAuth -> IO Integer
getBlockCount = callNumber "getblockcount" []

-- | Returns the block number of the latest block in the longest block chain
getBlockNumber :: BitcoinAuth -> IO Integer
getBlockNumber = callNumber "getblocknumber" []

-- | Returns the number of connections to other nodes
getConnectionCount :: BitcoinAuth -> IO Integer
getConnectionCount = callNumber "getconnectioncount" []

-- | Returns the proof-of-work difficulty as a multiple of the minimum
-- difficulty
getDifficulty :: BitcoinAuth -> IO Double
getDifficulty = callNumber "getdifficulty" []

-- | Indicates whether the node is generating or not
getGenerate :: BitcoinAuth -> IO Bool
getGenerate = callBool "getgenerate" []

-- | Returns a recent hashes per second performance measurement while
-- generating
getHashesPerSec :: BitcoinAuth -> IO Integer
getHashesPerSec = callNumber "gethashespersec" []

-- | Returns the total amount received by addresses with the given
-- account in transactions with at least 'minconf' confirmations
getReceivedByAccount :: BitcoinAuth
                     -> AccountName
                     -> MinConf
                     -> IO BitcoinAmount
getReceivedByAccount auth acct conf =
    callNumber "getreceivedbyaccount" [toValue acct,toValue conf] auth

-- | Returns the total amount received by an address in transactions
-- with at least 'minconf' confirmations.
getReceivedByAddress :: BitcoinAuth
                     -> Address
                     -> MinConf
                     -> IO BitcoinAmount
getReceivedByAddress auth addr conf =
    callNumber "getreceivedbyaddress" [toValue addr,toValue conf] auth

-- | Encapsulates address validation results from 'validateAddress'
data AddressValidation = AddressValidation
    { isValid :: Bool
    , isMine  :: Bool
    , account :: AccountName
    } deriving (Show)

-- | Return information about an address.
-- If the address is invalid or doesn't belong to us, the account name
-- is the empty string.
validateAddress :: BitcoinAuth
                -> Address
                -> IO AddressValidation
validateAddress auth addr = do
    (Object result) <- callBitcoinApi auth "validateaddress" [toValue addr]
    return AddressValidation
        { isValid = bool False "isvalid" result
        , isMine  = bool False "ismine"  result
        , account = str  ""    "account" result
        }
  where
    bool d k r = maybe d (\(Bool b)->b) $ M.lookup k r
    str  d k r = maybe d (\(String t)->T.unpack t) $ M.lookup k r

-- | Returns true if the RPC says the address is valid.
-- (This function only makes sense until 'mkAddress' does
-- full address verification)
isValidAddress :: BitcoinAuth -> Address -> IO Bool
isValidAddress auth addr = validateAddress auth addr >>= return . isValid
