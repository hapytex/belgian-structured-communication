{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Finance.Belgium.StructuredCommunication
  ( StructuredCommunication,
    structuredCommunication,
    checksum,
    communicationToText,
    parseCommunication,
    beCommunication,
  )
where

import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Data.Binary (Binary (get, put))
import Data.Char (digitToInt)
import Data.Data (Data)
import Data.Hashable (Hashable)
-- import Data.Either(either)
import Data.Text (Text, pack)
import Data.Typeable (Typeable)
import Data.Validity (Validity (validate), check, prettyValidate)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType))
import Language.Haskell.TH.Syntax (Code (Code), Exp (AppE, ConE, LitE), Lift (lift, liftTyped), Lit (IntegerL), Pat (ConP, LitP), TExp (TExp))
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck.Gen (chooseBoundedIntegral)
import Text.Parsec.Char (char, digit, space)
import Text.Parsec.Combinator (eof)
import Text.Parsec.Prim (ParsecT, Stream, runParser, skipMany, try)
import Text.Printf (printf)

data StructuredCommunication = StructuredCommunication
  { first :: !Word16,
    second :: !Word16,
    third :: !Word32
  }
  deriving (Data, Eq, Generic, Ord, Read, Typeable)

instance Show StructuredCommunication where
  show c = "[beCommunication|" <> communicationToString c <> "|]"

instance Hashable StructuredCommunication

instance Lift StructuredCommunication where
  liftTyped (StructuredCommunication v0 v1 v2) = Code (pure (TExp (ConE 'StructuredCommunication `AppE` f (fromIntegral v0) `AppE` f (fromIntegral v1) `AppE` f (fromIntegral v2))))
    where
      f = LitE . IntegerL
  lift (StructuredCommunication v0 v1 v2) = pure (ConE 'StructuredCommunication `AppE` f (fromIntegral v0) `AppE` f (fromIntegral v1) `AppE` f (fromIntegral v2))
    where
      f = LitE . IntegerL

checksum :: StructuredCommunication -> Word32
checksum (StructuredCommunication _ _ v₂) = v₂ `mod` 100

_rcheck :: Integral i => Integer -> i -> Bool
_rcheck mx = go
  where
    go v = 0 <= i && i <= mx where i = fromIntegral v

structuredCommunication :: (Integral i, Integral j, Integral k) => i -> j -> k -> Maybe StructuredCommunication
structuredCommunication v₀ v₁ v₂
  | _rcheck 999 v₀ && _rcheck 9999 v₁ && _rcheck 99948 v₂ && validChecksum s = Just s
  | otherwise = Nothing
  where
    s = StructuredCommunication (fromIntegral v₀) (fromIntegral v₁) (fromIntegral v₂)

instance Arbitrary StructuredCommunication where
  arbitrary = fixChecksum <$> (StructuredCommunication <$> chooseBoundedIntegral (0, 999) <*> chooseBoundedIntegral (0, 9999) <*> ((100 *) <$> chooseBoundedIntegral (0, 999)))

instance Bounded StructuredCommunication where
  minBound = fixChecksum (StructuredCommunication 0 0 0)
  maxBound = fixChecksum (StructuredCommunication 999 9999 99900)

instance Enum StructuredCommunication where
  fromEnum (StructuredCommunication v₀ v₁ v₂) = fromIntegral v₀ * 10000000 + fromIntegral v₁ * 1000 + fromIntegral (v₂ `div` 100)
  toEnum v = fixChecksum (StructuredCommunication (fromIntegral v₀) (fromIntegral v₁) (fromIntegral v₂))
    where
      v₂ = (v `mod` 1000) * 100
      v₁ = (v `div` 1000) `mod` 10000
      v₀ = v `div` 10000000

-- instance Enum StructuredCommunication where

instance Binary StructuredCommunication where
  get = StructuredCommunication <$> get <*> get <*> get
  put (StructuredCommunication v₀ v₁ v₂) = put v₀ >> put v₁ >> put v₂

instance Validity StructuredCommunication where
  validate s@(StructuredCommunication v₀ v₁ v₂) =
    check (v₀ <= 999) "first sequence larger has more than three digits."
      <> check (v₁ <= 9999) "second sequence larger has more than four digits."
      <> check (v₂ <= 99999) "third sequence larger has more than five digits."
      <> check (0 < c && c <= 97) "checksum out of the 1–97 range."
      <> check (determineCheckSum s == c) "checksum does not match."
    where
      c = checksum s

determineCheckSum :: StructuredCommunication -> Word32
determineCheckSum (StructuredCommunication v₀ v₁ v₂)
  | cs₂ == 0 = 97
  | otherwise = cs₂
  where
    cs₀ = v₀ `mod` 97
    cs₁ = (cs₀ * 9 + v₁) `mod` 97 -- 10000 `mod` 97 ==  9  (shift four decimal places)
    cs₂ = (fromIntegral cs₁ * 30 + v₂ `div` 100) `mod` 97 --  1000 `mod` 97 == 30  (shift three decimal places)

validChecksum :: StructuredCommunication -> Bool
validChecksum s@(StructuredCommunication _ _ v₂) = determineCheckSum s == v₂ `mod` 100

fixChecksum :: StructuredCommunication -> StructuredCommunication
fixChecksum s@(StructuredCommunication _ _ v₂) = s {third = v₂ - (v₂ `mod` 100) + determineCheckSum s}

communicationToString :: StructuredCommunication -> String
communicationToString (StructuredCommunication v₀ v₁ v₂) = "+++" <> printf "%03d" v₀ <> "/" <> printf "%04d" v₁ <> "/" <> printf "%05d" v₂ <> "+++"

communicationToText :: StructuredCommunication -> Text
communicationToText = pack . communicationToString

_parseNatWidth :: (Integral i, Stream s m Char) => Int -> ParsecT s u m i
_parseNatWidth m
  | m >= 0 = go m 0
  | otherwise = fail "negative number of digits"
  where
    go 0 v = pure v
    go n v = digit >>= go (n -1) . ((10 * v) +) . fromIntegral . digitToInt

_char3 :: Stream s m Char => Char -> ParsecT s u m Char
_char3 c = c' <* c' <* c'
  where
    c' = char c

_presuf :: Stream s m Char => ParsecT s u m Char
_presuf = try (_char3 '+') <|> (_char3 '*')

_slash :: Stream s m Char => ParsecT s u m Char
_slash = _space *> char '/' <* _space

_space :: Stream s m Char => ParsecT s u m ()
_space = skipMany space

parseCommunication :: Stream s m Char => ParsecT s u m StructuredCommunication
parseCommunication = do
  c <- _presuf <* _space
  c1 <- _parseNatWidth 3 <* _slash
  c2 <- _parseNatWidth 4 <* _slash
  c3 <- _parseNatWidth 5
  StructuredCommunication c1 c2 c3 <$ _space <* _char3 c

parseCommunication' :: Stream s m Char => ParsecT s u m StructuredCommunication
parseCommunication' = parseCommunication <* eof

_liftEither :: Show s => MonadFail m => Either s a -> m a
_liftEither = either (fail . show) pure

_toPattern :: StructuredCommunication -> Pat
_toPattern (StructuredCommunication v0 v1 v2) = ConP 'StructuredCommunication [] [f (fromIntegral v0), f (fromIntegral v1), f (fromIntegral v2)]
  where
    f = LitP . IntegerL

beCommunication :: QuasiQuoter
beCommunication =
  QuasiQuoter
    { quoteExp = (_liftEither >=> either fail pure . prettyValidate >=> lift) . runParser parseCommunication' () "",
      quotePat = (_liftEither >=> either fail pure . prettyValidate >=> pure . _toPattern) . runParser parseCommunication' () "",
      quoteType = const (fail "can not produce a type with this QuasiQuoter"),
      quoteDec = const (fail "can not produce a declaration with this QuasiQuoter")
    }
