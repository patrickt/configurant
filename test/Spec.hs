{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Configurant
import Control.Monad
import GHC.Generics (Generic)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

data Sample = Sample {numeric :: Int, textual :: String} deriving stock (Show, Eq, Generic)

example :: Config Sample
example = record @Sample ! #numeric (Configurant.read "INT_VALUE") ! #textual "STR_VALUE"

prop_simpleParsing :: Hedgehog.Property
prop_simpleParsing = Hedgehog.property do
  ival <- forAll (Gen.int Range.linearBounded)
  strval <- forAll (Gen.string (Range.linear 1 100) Gen.ascii)
  parsed <- evalEither . readConfig [("INT_VALUE", show ival), ("STR_VALUE", strval)] $ example
  parsed === Sample ival strval

main :: IO ()
main = void $ checkParallel $$(discover)
