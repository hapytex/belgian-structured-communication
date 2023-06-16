{-# LANGUAGE QuasiQuotes #-}

module Finance.Belgium.StructuredCommunicationSpec where

import Data.Validity(isValid)

import Finance.Belgium.StructuredCommunication(StructuredCommunication, beCommunication)

import Test.Hspec(Spec, it)
import Test.QuickCheck(maxSuccess, property, quickCheckWith, stdArgs)

_testEq :: (Integer -> Integer) -> (StructuredCommunication -> StructuredCommunication) -> Integer -> Bool
_testEq f g x' = fromInteger (f x) == g (fromInteger x)
  where x = abs x'

_testEq2 :: (Integer -> Integer -> Integer) -> (StructuredCommunication -> StructuredCommunication -> StructuredCommunication) -> Integer -> Integer -> Bool
_testEq2 f g x' y' = fromInteger (f x y) == g (fromInteger x) (fromInteger y)
  where x = abs x'
        y = abs y'

spec :: Spec
spec = do
  it "all random StructuredCommunications are valid" (property (isValid :: StructuredCommunication -> Bool))
  it "all succ's of a random StructuredCommunications are valid" (quickCheckWith stdArgs { maxSuccess = 1000000 } (property (isValid . succ :: StructuredCommunication -> Bool)))
  it "all pred's of a random StructuredCommunications are valid" (property (isValid . pred :: StructuredCommunication -> Bool))
  it "quasi quotation is valid" (isValid [beCommunication|+++000/0000/00097+++|])
  it "abs" (property (_testEq abs abs))
  it "negate" (property (_testEq negate negate))
  it "signum" (property (_testEq signum signum))
  it "(+)" (property (_testEq2 (+) (+)))
  it "(-)" (property (_testEq2 (-) (-)))
  it "(*)" (property (_testEq2 (*) (*)))
  -- it "quot" (property (_testEq2 quot quot))
  -- it "rem" (property (_testEq2 rem rem))
  -- it "div" (property (_testEq2 div div))
  -- it "mod" (property (_testEq2 mod mod))
  -- it "all are valid in an arbitrary range" (property (\x1 -> all isValid . enumFromTo x1 :: StructuredCommunication -> Bool))
  -- it "all StructuredCommunications can be parsed back" ())

patternMatch :: StructuredCommunication -> Bool
patternMatch [beCommunication|+++999/9999/99948+++|] = True
patternMatch _ = False
