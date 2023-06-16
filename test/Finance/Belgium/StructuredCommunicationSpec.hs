{-# LANGUAGE QuasiQuotes #-}

module Finance.Belgium.StructuredCommunicationSpec where

import Data.Validity(isValid)

import Finance.Belgium.StructuredCommunication(StructuredCommunication, beCommunication)

import Test.Hspec(Spec, it)
import Test.QuickCheck(maxSuccess, property, quickCheckWith, stdArgs)

spec :: Spec
spec = do
  it "all random StructuredCommunications are valid" (property (isValid :: StructuredCommunication -> Bool))
  it "all succ's of a random StructuredCommunications are valid" (quickCheckWith stdArgs { maxSuccess = 1000000 } (property (isValid . succ :: StructuredCommunication -> Bool)))
  it "all pred's of a random StructuredCommunications are valid" (property (isValid . pred :: StructuredCommunication -> Bool))
  it "quasi quotation is valid" (isValid [beCommunication|+++000/0000/00097+++|])
  -- it "all are valid in an arbitrary range" (property (\x1 -> all isValid . enumFromTo x1 :: StructuredCommunication -> Bool))
  -- it "all StructuredCommunications can be parsed back" ())

patternMatch :: StructuredCommunication -> Bool
patternMatch [beCommunication|+++999/9999/99948+++|] = True
patternMatch _ = False
