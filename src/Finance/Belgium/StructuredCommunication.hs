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

import Language.Haskell.TH.Quote(QuasiQuoter(QuasiQuoter))

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary))
import Test.QuickCheck.Gen(chooseBoundedIntegral)

import Text.Parsec.Char(char, digit, space)
import Text.Parsec.Combinator(eof)
import Text.Parsec.Prim(ParsecT, Stream, skipMany, try)
import Text.Printf(printf)

data StructuredCommunication
  = StructuredCommunication {
    first :: !Word16
  , second :: !Word16
  , third :: !Word32
  } deriving (Eq, Ord, Read, Show)

checksum :: StructuredCommunication -> Word32
checksum (StructuredCommunication _ _ v₂) = v₂ `mod` 100

_rcheck :: Integral i => Integer -> i -> Bool
_rcheck mx = go
  where go v = 0 <= i && i <= mx where i = fromIntegral v

structuredCommunication :: (Integral i, Integral j, Integral k) => i -> j -> k -> Maybe StructuredCommunication
structuredCommunication v₀ v₁ v₂
  | _rcheck 999 v₀ && _rcheck 9999 v₁ && _rcheck 99948 v₂ && validChecksum s = Just s
  | otherwise = Nothing
  where s = StructuredCommunication (fromIntegral v₀) (fromIntegral v₁) (fromIntegral v₂)

instance Arbitrary StructuredCommunication where
  arbitrary = fixChecksum <$> (StructuredCommunication <$> chooseBoundedIntegral (0, 999) <*> chooseBoundedIntegral (0, 9999) <*> ((100*) <$> chooseBoundedIntegral (0, 999)))

instance Bounded StructuredCommunication where
  minBound = fixChecksum (StructuredCommunication 0 0 0)
  maxBound = fixChecksum (StructuredCommunication 999 9999 99900)

instance Enum StructuredCommunication where
  fromEnum (StructuredCommunication v₀ v₁ v₂) = fromIntegral v₀ * 10000000 + fromIntegral v₁ * 1000 + fromIntegral (v₂ `div` 100)
  toEnum v = fixChecksum (StructuredCommunication (fromIntegral v₀) (fromIntegral v₁) (fromIntegral v₂))
    where v₂ = (v `mod` 1000) * 100
          v₁ = (v `div` 1000) `mod` 10000
          v₀ = v `div` 10000000

-- instance Enum StructuredCommunication where

instance Binary StructuredCommunication where
  get = StructuredCommunication <$> get <*> get <*> get
  put (StructuredCommunication v₀ v₁ v₂) = put v₀ >> put v₁ >> put v₂

instance Validity StructuredCommunication where
  validate s@(StructuredCommunication v₀ v₁ v₂) =
         check (v₀ <= 999)   "first sequence larger has more than three digits."
      <> check (v₁ <= 9999)  "second sequence larger has more than four digits."
      <> check (v₂ <= 99999) "third sequence larger has more than five digits."
      <> check (0 < c && c <= 97) "checksum out of the 1–97 range."
      <> check (determineCheckSum s == c) "checksum does not match."
        where c = checksum s

determineCheckSum :: StructuredCommunication -> Word32
determineCheckSum (StructuredCommunication v₀ v₁ v₂)
  | cs₂ == 0 = 97
  | otherwise = cs₂
  where cs₀ = v₀ `mod` 97
        cs₁ = (cs₀ * 9 + v₁) `mod` 97                            -- 10000 `mod` 97 ==  9  (shift four decimal places)
        cs₂ = (fromIntegral cs₁ * 30 + v₂ `div` 100) `mod` 97    --  1000 `mod` 97 == 30  (shift three decimal places)

validChecksum :: StructuredCommunication -> Bool
validChecksum s@(StructuredCommunication _ _ v₂) = determineCheckSum s == v₂ `mod` 100


fixChecksum :: StructuredCommunication -> StructuredCommunication
fixChecksum s@(StructuredCommunication _ _ v₂) = s { third=v₂ - (v₂ `mod` 100) + determineCheckSum s }

communicationToText :: StructuredCommunication -> Text
communicationToText (StructuredCommunication v₀ v₁ v₂) = "+++" <> p "%03d" v₀ <> "/" <> p "%04d" v₁ <> "/" <> p "%05d" v₂ <> "+++"
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

parseCommunication' :: Stream s m Char => ParsecT s u m StructuredCommunication
parseCommunication' = parseCommunication <* eof

beCommunicaton :: QuasiQuoter
beCommunicaton = QuasiQuoter {
    -- quoteExp = parseCommunication'
  }
