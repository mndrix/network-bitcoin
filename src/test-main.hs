{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck
import Data.Attoparsec.Number
import Network.Bitcoin
import Data.Ratio
import Data.Fixed
import Text.Printf

-- Creates an amount with the specified number of satoshis
satoshis :: Integer -> Ratio Integer
satoshis = (%resolution (Fixed Satoshi))

instance Arbitrary Number where
    arbitrary = fmap (D . fromRational . satoshis . abs) arbitrarySizedIntegral

fromNumberRoundTrip :: Number -> Bool
fromNumberRoundTrip n = (origString n) == rtString
  where
    trimZeros = reverse . dropWhile (=='0') . reverse
    origString (I i) = show i
    origString (D d) = trimZeros $ printf "%.8f" d
    rtString = trimZeros $ show (fromNumber n :: Amount)

main = do
    result <- quickCheckResult fromNumberRoundTrip
    case result of
        Success{} -> exitSuccess
        GaveUp{}  -> exitFailure
        Failure{} -> exitFailure
        NoExpectedFailure{} -> exitSuccess
