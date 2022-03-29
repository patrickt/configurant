{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Configurant.Internal
  ( module Configurant.Internal
  )
where

import Control.Applicative
import Data.Function
import Data.Generic.HKD
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString (..))
import Data.Validation (Validation)
import Data.Validation qualified as Validation
import GHC.Stack
import System.Environment (getEnvironment)
import Text.Read (readMaybe)
import Prelude hiding (read)

type Key = String

data Spec a where
  Keyed :: Spec a -> Key -> Spec a
  String :: Spec String
  Read :: Read a => Spec a
  Fail :: HasCallStack => Spec a

deriving stock instance Show (Spec a)

-- I'd like Functor, Applicative, and Alternative for Spec, but it makes the validator a little fraught

instance IsString (Spec String) where
  fromString = Keyed String

data Error
  = NoValueForKey Key String
  | ValidationError CallStack
  | Unkeyed String
  | BadFormat String String
  deriving stock (Show)

type Errors = NonEmpty Error

type Env = [(String, String)]

read :: Read a => Key -> Spec a
read = Keyed Read

die :: Error -> Validation Errors a
die = Validation.Failure . pure

toValidation :: forall a. Spec a -> Env -> Validation Errors a
toValidation s env = case s of
  Fail -> die (ValidationError callStack)
  Keyed sub key -> case sub of
    String -> lookup key env & maybe (die (NoValueForKey key (show sub))) pure
    Read -> case fmap readMaybe (lookup key env) of
      Nothing -> die (NoValueForKey key (show sub))
      Just Nothing -> die (BadFormat key "TODO") -- todo: report value
      Just (Just v) -> pure v
    other -> toValidation other env
  Read -> die $ Unkeyed "readable"
  String -> die $ Unkeyed "string"

type Config a = HKD a Spec

readEnv :: (FunctorB (HKD a), Construct (Validation Errors) a) => Config a -> IO (Either Errors a)
readEnv c = flip readConfig c <$> getEnvironment

readConfig :: (FunctorB (HKD a), Construct (Validation Errors) a) => Env -> Config a -> Either Errors a
readConfig e = Validation.toEither . construct . bmap (`toValidation` e)
