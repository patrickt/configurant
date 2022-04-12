{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Configurant
import Control.Monad
import GHC.Generics (Generic)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Exit (die)

data Sample = Sample {numeric :: Int, textual :: String} deriving stock (Show, Eq, Generic)

example :: Config Sample
example = record @Sample ! #numeric (Configurant.read "INT_VALUE") ! #textual "STR_VALUE"

prop_simpleParsing :: Hedgehog.Property
prop_simpleParsing = Hedgehog.property do
  ival <- forAll (Gen.int Range.linearBounded)
  strval <- forAll (Gen.string (Range.linear 1 100) Gen.ascii)
  parsed <- evalEither . fromPairs [("INT_VALUE", show ival), ("STR_VALUE", strval)] $ example
  parsed === Sample ival strval

prop_handleMissing :: Hedgehog.Property
prop_handleMissing = Hedgehog.property do
  let failing = fromPairs [] example
  failing === Left [NoValueForKey "INT_VALUE" "Int value", NoValueForKey "STR_VALUE" "string value"]

main :: IO ()
main = do
  ok <-
    checkParallel $
      Group
        "Test.Example"
        [ ("simple parsing", prop_simpleParsing),
          ("missing values", prop_handleMissing)
        ]
  unless ok (die "failed")
