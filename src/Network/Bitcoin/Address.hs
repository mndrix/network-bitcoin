module Network.Bitcoin.Address
    (
    -- * Types
      Address

    -- * Functions
    , mkAddress
    )
where

-- | Represents a Bitcoin receiving address.  Construct one with
-- 'mkAddress'.
data Address = Address String
instance Show Address where
    show (Address s) = s

-- | Construct an 'Address' from a 'String'.
-- Returns 'Nothing' if the string is not a valid Bitcoin address.
--
-- Only validates approximate address format.
-- /Does not/ validate address checksum.
-- Until full validation is done, use 'isValidAddress' RPC call instead
mkAddress :: String -> Maybe Address
mkAddress s =
    if isOK s
        then Just $ Address s
        else Nothing
  where -- TODO validate address checksum (write base58 module first)
    isOK ('1':_) = (length s >= 25) && (length s <= 34)
    isOK _       = False
