{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- |
-- Module      : Finance.Belgium.StructuredCommunication
-- Description : A module to parse, render and manipulate Belgian structured communication for financial transactions.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Belgian companies often make use of /structured communication/ with a checksum. This package aims to provide a toolkit to parse, render and manipulate 'StructuredCommunication'.
module Finance.Belgium.StructuredCommunication
  ( -- * Constructing 'StructuredCommunication'
    StructuredCommunication(StructuredCommunication),
    structuredCommunication,

    -- * determining the checksum
    checksum,
    determineChecksum,
    validChecksum,
    fixChecksum,

    -- * Converting to text
    communicationToString,
    communicationToText,

    -- * Parsing from text
    communicationParser,
    communicationParser',
    communicationEParser,
    communicationEParser',
    parseCommunication,
    parseCommunication',
    parseCommunicationE,
    parseCommunicationE',

    -- * Quasi quotation
    beCommunication,
  )
where

import Control.Applicative ((<|>))
import Control.Monad ((>=>))
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail(MonadFail)
#endif
import Data.Binary (Binary (get, put))
import Data.Char (digitToInt)
import Data.Data (Data)
import Data.Functor.Identity(Identity)
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Text (Text, pack)
import Data.Typeable (Typeable)
#if MIN_VERSION_validity(0,9,0)
import Data.Validity (Validity (validate), check, prettyValidate)
#else
import Data.Validity (Validation(Validation), Validity (validate), check)
#endif
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType))
#if MIN_VERSION_template_haskell(2, 17, 0)
import Language.Haskell.TH.Syntax (Code (Code), Exp (AppE, ConE, LitE), Lift (lift, liftTyped), Lit (IntegerL), Pat (ConP, LitP), TExp (TExp))
#elif MIN_VERSION_template_haskell(2, 16, 0)
import Language.Haskell.TH.Syntax (Exp (AppE, ConE, LitE), Lift (lift, liftTyped), Lit (IntegerL), Pat (ConP, LitP), TExp (TExp))
#else
import Language.Haskell.TH.Syntax (Exp (AppE, ConE, LitE), Lift (lift), Lit (IntegerL), Pat (ConP, LitP))
#endif
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck.Gen (choose)
import Text.Parsec(ParseError)
import Text.Parsec.Char (char, digit, space)
import Text.Parsec.Combinator (eof)
import Text.Parsec.Prim (ParsecT, Stream, runParser, skipMany, try)
import Text.Printf (printf)

-- | A data type that stores three numbers: one with three digits (@000–999@), four digits (@0000–9999@) and five digits (@00001–99997@). The data
-- constructor itself is not accessible, since the `StructuredCommunication` could produce objects that are out of the given ranges, or where the
-- checksum is not valid. The module thus aims to prevent parsing, changing, etc. 'StructuredCommunication' objects into an invalid state.
data StructuredCommunication = StructuredCommunication !Word16 !Word16 !Word32 deriving (Data, Eq, Generic, Ord, Read, Typeable)

_maxVal :: Integral a => a
_maxVal = 9999999999

_numVals :: Integral a => a
_numVals = 10000000000

_fromEnum :: StructuredCommunication -> Int64
_fromEnum (StructuredCommunication v₀ v₁ v₂) = fromIntegral v₀ * 10000000 + fromIntegral v₁ * 1000 + fromIntegral (v₂ `div` 100)

_toEnum :: Int64 -> StructuredCommunication
_toEnum v = fixChecksum (StructuredCommunication (fromIntegral v₀) (fromIntegral v₁) (fromIntegral v₂))
  where
    v₂ = (v `mod` 1000) * 100
    v₁ = (v `div` 1000) `mod` 10000
    v₀ = v `div` 10000000

instance Num StructuredCommunication where
  fromInteger = _toEnum . fromInteger . (`mod` _numVals)
  v1 + v2 = _toEnum ((_fromEnum v1 + _fromEnum v2) `mod` _numVals)
  v1 - v2 = _toEnum ((_fromEnum v1 - _fromEnum v2) `mod` _numVals)
  negate = _toEnum . (`mod` _numVals) . negate . _fromEnum
  abs = id
  signum 0 = 0
  signum _ = 1
  v1' * v2' = _toEnum ((m1 * v2 + (v1 - m1) * m2) `mod` _numVals)
    where
      v1 = _fromEnum v1'
      v2 = _fromEnum v2'
      m1 = v1 `mod` 100000
      m2 = v2 `mod` 100000

_both :: (a -> b) -> (a, a) -> (b, b)
_both f ~(x, y) = (f x, f y)

instance Real StructuredCommunication where
  toRational = toRational . toInteger

instance Integral StructuredCommunication where
  toInteger = toInteger . _fromEnum
  quot x = _toEnum . quot (_fromEnum x) . _fromEnum
  rem x = _toEnum . rem (_fromEnum x) . _fromEnum
  quotRem x = _both _toEnum . quotRem (_fromEnum x) . _fromEnum
  div x = _toEnum . div (_fromEnum x) . _fromEnum
  mod x = _toEnum . mod (_fromEnum x) . _fromEnum
  divMod x = _both _toEnum . quotRem (_fromEnum x) . _fromEnum

instance Show StructuredCommunication where
  show c = "[beCommunication|" ++ communicationToString c ++ "|]"

instance Hashable StructuredCommunication

-- | Determining the /checksum/-part for the given 'StructuredCommunication'. This thus takes the last two digits, or the third number modulo one hundred.
checksum ::
  -- | The 'StructuredCommunication' for which we determine the checkum.
  StructuredCommunication ->
  -- | The last two digits of the 'StructuredCommunication' object. The checksum is /not/ per se valid.
  Word32
checksum (StructuredCommunication _ _ v₂) = v₂ `mod` 100

_rcheck :: Integral i => Integer -> i -> Bool
_rcheck mx = go
  where
    go v = 0 <= i && i <= mx where i = fromIntegral v

-- | Construct a 'StructuredCommunication' object for the given three integral values that form the three sequences of digits.
structuredCommunication ::
  (Integral i, Integral j, Integral k) =>
  -- | The first number, should be between @000@ and @999@.
  i ->
  -- | The second number, should be between @0000@ and @9999@.
  j ->
  -- | The third number, should be between @00001@ and @99997@.
  k ->
  -- | The 'StructuredCommunication' wrapped in a 'Just' of the three numbers are in range, and the checksum matches, otherwise 'Nothing'.
  Maybe StructuredCommunication
structuredCommunication v₀ v₁ v₂
  | _rcheck 999 v₀ && _rcheck 9999 v₁ && _rcheck 99997 v₂ && validChecksum s = Just s
  | otherwise = Nothing
  where
    s = StructuredCommunication (fromIntegral v₀) (fromIntegral v₁) (fromIntegral v₂)

instance Arbitrary StructuredCommunication where
  arbitrary = fixChecksum <$> (StructuredCommunication <$> choose (0, 999) <*> choose (0, 9999) <*> ((100 *) <$> choose (0, 999)))

instance Bounded StructuredCommunication where
  minBound = fixChecksum (StructuredCommunication 0 0 0)
  maxBound = fixChecksum (StructuredCommunication 999 9999 99900)

instance Enum StructuredCommunication where
  fromEnum = fromIntegral . _fromEnum
  toEnum = _toEnum . fromIntegral
  succ = _toEnum . succ . _fromEnum
  pred = _toEnum . pred . _fromEnum
  enumFrom v = map _toEnum [_fromEnum v .. _maxVal]
  enumFromThen v₀ v₁
    | v₀ <= v₁ = map _toEnum [_fromEnum v₀, _fromEnum v₁ .. _maxVal]
    | otherwise = map _toEnum [_fromEnum v₀, _fromEnum v₁ .. 0]
  enumFromTo v₀ v₁ = map _toEnum [_fromEnum v₀ .. _fromEnum v₁]
  enumFromThenTo v₀ v₁ v₂ = map _toEnum [_fromEnum v₀, _fromEnum v₁ .. _fromEnum v₂]

instance Binary StructuredCommunication where
  get = StructuredCommunication <$> get <*> get <*> get
  put (StructuredCommunication v₀ v₁ v₂) = put v₀ >> put v₁ >> put v₂

instance Validity StructuredCommunication where
  validate s@(StructuredCommunication v₀ v₁ v₂) =
    check (v₀ <= 999) "first sequence larger has more than three digits."
      `mappend` check (v₁ <= 9999) "second sequence larger has more than four digits."
      `mappend` check (v₂ <= 99999) "third sequence larger has more than five digits."
      `mappend` check (0 < c && c <= 97) "checksum out of the 1–97 range."
      `mappend` check (determineChecksum s == c) "checksum does not match."
    where
      c = checksum s

-- | Determine the checksum based on the first ten digits. If the 'StructuredCommunication' is not valid, its 'checksum' will /not/ match the result of the 'determineChecksum'.
determineChecksum ::
  -- | The 'StructuredCommunication' to determine the /checksum/ from.
  StructuredCommunication ->
  -- | The checksum determined by the first ten digits, not per se the /real/ checksum of the 'StructuredCommunication'.
  Word32
determineChecksum (StructuredCommunication v₀ v₁ v₂)
  | cs₂ == 0 = 97
  | otherwise = cs₂
  where
    cs₀ = v₀ `mod` 97
    cs₁ = (cs₀ * 9 + v₁) `mod` 97 -- 10000 `mod` 97 ==  9  (shift four decimal places)
    cs₂ = (fromIntegral cs₁ * 30 + v₂ `div` 100) `mod` 97 --  1000 `mod` 97 == 30  (shift three decimal places)

-- | Check if the checksum matches for the given 'StructuredCommunication'.
validChecksum ::
  -- | The 'StructuredCommunication' for which we check the checksum.
  StructuredCommunication ->
  -- | 'True' if the checksum is valid; 'False' otherwise.
  Bool
validChecksum s@(StructuredCommunication _ _ v₂) = determineChecksum s == v₂ `mod` 100

-- | Convert the given 'StructuredCommunication' to one where the checksum is valid. If the checksum was already valid, it returns an equivalent
-- 'StructuredCommunication', this operation is thus /idempotent/.
fixChecksum ::
  -- | The given 'StructuredCommunication' to fix.
  StructuredCommunication ->
  -- | A variant of the given 'StructuredCommunication' where only the last two digits are changed to have a valid checksum.
  StructuredCommunication
fixChecksum s@(StructuredCommunication v₀ v₁ v₂) = StructuredCommunication v₀ v₁ (v₂ - (v₂ `mod` 100) + determineChecksum s)

-- | Convert the given 'StructuredCommunication' to a 'String' that looks like a structured communication, so @+++000\/0000\/00097+++@.
communicationToString ::
  -- | The given 'StructuredCommunication' to convert to a 'String'.
  StructuredCommunication ->
  -- | The corresponding 'String', of the form @+++000\/0000\/00097+++@.
  String
communicationToString (StructuredCommunication v₀ v₁ v₂) = "+++" ++ printf "%03d" v₀ ++ "/" ++ printf "%04d" v₁ ++ "/" ++ printf "%05d" v₂ ++ "+++"

-- | Convert the given 'StructuredCommunication' to a 'Text' that looks like a structured communication, so @+++000\/0000\/00097+++@.
communicationToText ::
  -- | The given 'StructuredCommunication' to convert to a 'Text'.
  StructuredCommunication ->
  -- | The corresponding 'Text', of the form @+++000\/0000\/00097+++@.
  Text
communicationToText = pack . communicationToString

_parseNatWidth :: (Integral i, Stream s m Char) => Int -> ParsecT s u m i
_parseNatWidth m
  | m >= 0 = go m 0
  | otherwise = fail "negative number of digits"
  where
    go 0 v = pure v
    go n v = digit >>= go (n - 1) . ((10 * v) +) . fromIntegral . digitToInt

_char3 :: Stream s m Char => Char -> ParsecT s u m Char
_char3 c = c' <* c' <* c'
  where
    c' = char c

_presuf :: Stream s m Char => ParsecT s u m Char
_presuf = try (_char3 '+') <|> _char3 '*'

_slash :: Stream s m Char => ParsecT s u m Char
_slash = _space *> char '/' <* _space

_space :: Stream s m Char => ParsecT s u m ()
_space = skipMany space

-- | A 'ParsecT' that parses a string into a 'StructuredCommunication', the 'StructuredCommunication' can be invalid. The parser also does /not/ (per se) ends with an 'eof'.
communicationParser' ::
  Stream s m Char =>
  -- | The 'ParsecT' object that parses the structured communication of the form @+++000\/0000\/00097+++@.
  ParsecT s u m StructuredCommunication
communicationParser' = do
  c <- _presuf <* _space
  c1 <- _parseNatWidth 3 <* _slash
  c2 <- _parseNatWidth 4 <* _slash
  c3 <- _parseNatWidth 5
  StructuredCommunication c1 c2 c3 <$ _space <* _char3 c

-- | A 'ParsecT' that parses a string into a 'StructuredCommunication', the 'StructuredCommunication' is checked for its validity (checksum). The parser does /not/ (per se) ends with an 'eof'.
communicationParser ::
  Stream s m Char =>
  -- | The 'ParsecT' object that parses the structured communication of the form @+++000\/0000\/00097+++@.
  ParsecT s u m StructuredCommunication
communicationParser = communicationParser' >>= _liftEither . prettyValidate

-- | A 'ParsecT' that parses a string into a 'StructuredCommunication', the 'StructuredCommunication' can be invalid. The parser also checks if this is the end of the stream.
communicationEParser' ::
  Stream s m Char =>
  -- | The 'ParsecT' object that parses the structured communication of the form @+++000\/0000\/00097+++@.
  ParsecT s u m StructuredCommunication
communicationEParser' = communicationParser <* eof

-- | A 'ParsecT' that parses a string into a 'StructuredCommunication', the 'StructuredCommunication' is checked for its validity (checksum). The parser also checks that this is the end of the stream.
communicationEParser ::
  Stream s m Char =>
  -- | The 'ParsecT' object that parses the structured communication of the form @+++000\/0000\/00097+++@.
  ParsecT s u m StructuredCommunication
communicationEParser = communicationEParser' >>= _liftEither . prettyValidate

-- | Parsing a stream into a 'StructuredCommunication' that also validates the checksum of the communication. The stream does not per se needs to end with structured communcation.
parseCommunication :: Stream s Identity Char =>
  -- | The stream that is parsed into a 'StructuredCommunication'
  s ->
  -- | The result of parsing, either a 'StructuredCommunication' wrapped in a 'Right' or a parsing error wrapped in a 'Left'.
  Either ParseError StructuredCommunication
parseCommunication = runParser communicationParser () ""

-- | Parsing a stream into a 'StructuredCommunication' that does /noet/ validate the checksum of the communication. The stream does not per se needs to end with structured communcation.
parseCommunication' :: Stream s Identity Char =>
  -- | The stream that is parsed into a 'StructuredCommunication'
  s ->
  -- | The result of parsing, either a 'StructuredCommunication' wrapped in a 'Right' or a parsing error wrapped in a 'Left'.
  Either ParseError StructuredCommunication
parseCommunication' = runParser communicationParser' () ""

-- | Parsing a stream into a 'StructuredCommunication' that also validates the checksum of the communication. After the structured communication, the stream needs to end.
parseCommunicationE :: Stream s Identity Char =>
  -- | The stream that is parsed into a 'StructuredCommunication'
  s ->
  -- | The result of parsing, either a 'StructuredCommunication' wrapped in a 'Right' or a parsing error wrapped in a 'Left'.
  Either ParseError StructuredCommunication
parseCommunicationE = runParser communicationEParser () ""

-- | Parsing a stream into a 'StructuredCommunication' that does /noet/ validate the checksum of the communication. After the structured communication, the stream needs to end.
parseCommunicationE' :: Stream s Identity Char =>
  -- | The stream that is parsed into a 'StructuredCommunication'
  s ->
  -- | The result of parsing, either a 'StructuredCommunication' wrapped in a 'Right' or a parsing error wrapped in a 'Left'.
  Either ParseError StructuredCommunication
parseCommunicationE' = runParser communicationEParser' () ""


_liftEither :: Show s => MonadFail m => Either s a -> m a
_liftEither = either (fail . show) pure

_toPattern :: StructuredCommunication -> Pat

#if MIN_VERSION_template_haskell(2, 18, 0)
_toPattern (StructuredCommunication v₀ v₁ v₂) = ConP 'StructuredCommunication [] [f (fromIntegral v₀), f (fromIntegral v₁), f (fromIntegral v₂)]
  where
    f = LitP . IntegerL
#else
_toPattern (StructuredCommunication v₀ v₁ v₂) = ConP 'StructuredCommunication [f (fromIntegral v₀), f (fromIntegral v₁), f (fromIntegral v₂)]
  where
    f = LitP . IntegerL
#endif

#if !MIN_VERSION_validity(0,9,0)
prettyValidate :: Validity a => a -> Either String a
prettyValidate a = go (validate a)
  where go (Validation []) = Right a
        go v = Left (show v)
#endif

-- | A 'QuasiQuoter' that can parse a string into an expression or pattern. It will thus convert @+++000\/000\/00097+++@ into a 'StructuredCommunication' as expression or pattern.
beCommunication ::
  -- | A 'QuasiQuoter' to parse to a 'StructuredCommunication'.
  QuasiQuoter
beCommunication =
  QuasiQuoter
    { quoteExp = (_liftEither >=> lift) . runParser communicationEParser () "",
      quotePat = (_liftEither >=> pure . _toPattern) . runParser communicationEParser () "",
      quoteType = const (fail "can not produce a type with this QuasiQuoter"),
      quoteDec = const (fail "can not produce a declaration with this QuasiQuoter")
    }

instance Lift StructuredCommunication where
  lift (StructuredCommunication v₀ v₁ v₂) = pure (ConE 'StructuredCommunication `AppE` f (fromIntegral v₀) `AppE` f (fromIntegral v₁) `AppE` f (fromIntegral v₂))
    where
      f = LitE . IntegerL

#if MIN_VERSION_template_haskell(2, 17, 0)
  liftTyped (StructuredCommunication v₀ v₁ v₂) = Code (pure (TExp (ConE 'StructuredCommunication `AppE` f (fromIntegral v₀) `AppE` f (fromIntegral v₁) `AppE` f (fromIntegral v₂))))
    where
      f = LitE . IntegerL
#elif MIN_VERSION_template_haskell(2, 16, 0)
  liftTyped (StructuredCommunication v₀ v₁ v₂) = pure (TExp (ConE 'StructuredCommunication `AppE` f (fromIntegral v₀) `AppE` f (fromIntegral v₁) `AppE` f (fromIntegral v₂)))
    where
      f = LitE . IntegerL
#endif
