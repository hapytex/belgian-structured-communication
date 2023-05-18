{-# LANGUAGE OverloadedStrings #-}

module Finance.Belgium.StructuredCommunication where

import Data.Binary(Binary(get, put))
import Data.Text(Text, pack)
import Data.Validity(Validity(validate), check)
import Data.Word(Word16, Word32)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary))
import Test.QuickCheck.Gen(chooseBoundedIntegral)

import Text.Printf(printf)

data StructuredCommunication
  = StructuredCommunication {
    first :: !Word16
  , second :: !Word16
  , third :: !Word32
  } deriving (Eq, Ord, Read, Show)

instance Arbitrary StructuredCommunication where
  arbitrary = fixChecksum <$> (StructuredCommunication <$> chooseBoundedIntegral (0, 999) <*> chooseBoundedIntegral (0, 9999) <*> ((100*) <$> chooseBoundedIntegral (0, 999)))

instance Binary StructuredCommunication where
  get = StructuredCommunication <$> get <*> get <*> get
  put (StructuredCommunication v0 v1 v2) = put v0 >> put v1 >> put v2

instance Validity StructuredCommunication where
  validate s@(StructuredCommunication v0 v1 v2) =
         check (v0 <= 999)   "first sequence larger has more than three digits."
      <> check (v1 <= 9999)  "second sequence larger has more than four digits."
      <> check (v2 <= 99999) "third sequence larger has more than five digits."
      <> check (0 < c && c <= 97) "checksum out of the 1–97 range."
      <> check (determineCheckSum s == c) "checksum does not match."
    where c = v2 `mod` 100

determineCheckSum :: StructuredCommunication -> Word32
determineCheckSum (StructuredCommunication v0 v1 v2)
  | cs2 == 0 = 97
  | otherwise = cs2
  where cs0 = v0 `mod` 97
        cs1 = (cs0 * 9 + v1) `mod` 97                            -- 10000 `mod` 97 ==  9  (shift four decimal places)
        cs2 = (fromIntegral cs1 * 30 + v2 `div` 100) `mod` 97    --  1000 `mod` 97 == 30  (shift three decimal places)

fixChecksum :: StructuredCommunication -> StructuredCommunication
fixChecksum s@(StructuredCommunication _ _ v2) = s { third=v2 - (v2 `mod` 100) + determineCheckSum s }

communicationToText :: StructuredCommunication -> Text
communicationToText (StructuredCommunication v0 v1 v2) = "+++" <> p "%03d" v0 <> "/" <> p "%04d" v1 <> "/" <> p "%05d" v2 <> "+++"
  where p f = pack . printf f
