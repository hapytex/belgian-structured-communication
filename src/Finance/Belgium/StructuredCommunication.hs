{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Finance.Belgium.StructuredCommunication (
    StructuredCommunication
  , structuredCommunication
  , checksum
  , communicationToText, parseCommunication
  ) where

import Control.Applicative((<|>))
import Control.Monad(replicateM_)

import Data.Binary(Binary(get, put))
import Data.Char(digitToInt)
import Data.Text(Text, pack)
import Data.Validity(Validity(validate), check)
import Data.Word(Word16, Word32)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary))
import Test.QuickCheck.Gen(chooseBoundedIntegral)

import Text.Parsec.Char(char, digit, space)
import Text.Parsec.Prim(ParsecT, Stream, skipMany, try)
import Text.Printf(printf)

data StructuredCommunication
  = StructuredCommunication {
    first :: !Word16
  , second :: !Word16
  , third :: !Word32
  } deriving (Eq, Ord, Read, Show)

checksum :: StructuredCommunication -> Word32
checksum (StructuredCommunication _ _ v2) = v2 `mod` 100

_rcheck :: Integral i => Integer -> i -> Bool
_rcheck mx = go
  where go v = 0 <= i && i <= mx where i = fromIntegral v

structuredCommunication :: (Integral i, Integral j, Integral k) => i -> j -> k -> Maybe StructuredCommunication
structuredCommunication v0 v1 v2
  | _rcheck 999 v0 && _rcheck 9999 v1 && _rcheck 99948 v2 && validChecksum s = Just s
  | otherwise = Nothing
  where s = StructuredCommunication (fromIntegral v0) (fromIntegral v1) (fromIntegral v2)

instance Arbitrary StructuredCommunication where
  arbitrary = fixChecksum <$> (StructuredCommunication <$> chooseBoundedIntegral (0, 999) <*> chooseBoundedIntegral (0, 9999) <*> ((100*) <$> chooseBoundedIntegral (0, 999)))

instance Bounded StructuredCommunication where
  minBound = fixChecksum (StructuredCommunication 0 0 0)
  maxBound = fixChecksum (StructuredCommunication 999 9999 99900)

-- instance Enum StructuredCommunication where

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
        where c = checksum s

determineCheckSum :: StructuredCommunication -> Word32
determineCheckSum (StructuredCommunication v0 v1 v2)
  | cs2 == 0 = 97
  | otherwise = cs2
  where cs0 = v0 `mod` 97
        cs1 = (cs0 * 9 + v1) `mod` 97                            -- 10000 `mod` 97 ==  9  (shift four decimal places)
        cs2 = (fromIntegral cs1 * 30 + v2 `div` 100) `mod` 97    --  1000 `mod` 97 == 30  (shift three decimal places)

validChecksum :: StructuredCommunication -> Bool
validChecksum s@(StructuredCommunication _ _ v2) = determineCheckSum s == v2 `mod` 100


fixChecksum :: StructuredCommunication -> StructuredCommunication
fixChecksum s@(StructuredCommunication _ _ v2) = s { third=v2 - (v2 `mod` 100) + determineCheckSum s }

communicationToText :: StructuredCommunication -> Text
communicationToText (StructuredCommunication v0 v1 v2) = "+++" <> p "%03d" v0 <> "/" <> p "%04d" v1 <> "/" <> p "%05d" v2 <> "+++"
  where p f = pack . printf f

_parseNatWidth :: (Integral i, Stream s m Char) => Int -> ParsecT s u m i
_parseNatWidth m
  | m >= 0 = go m 0
  | otherwise = fail "negative number of digits"
  where go 0 v = pure v
        go n v = digit >>= go (n-1) . ((10*v)+) . fromIntegral . digitToInt

_presuf :: Stream s m Char => ParsecT s u m ()
_presuf = try (replicateM_ 3 (char '+')) <|> replicateM_ 3 (char '*')

_slash :: Stream s m Char => ParsecT s u m Char
_slash = _space *> char '/' <* _space

_space :: Stream s m Char => ParsecT s u m ()
_space = skipMany space

parseCommunication :: Stream s m Char => ParsecT s u m StructuredCommunication
parseCommunication = StructuredCommunication <$> (_presuf *> _space *> _parseNatWidth 3 <* _slash) <*> (_parseNatWidth 4 <* _slash) <*> (_parseNatWidth 5 <* _space <* _presuf)
