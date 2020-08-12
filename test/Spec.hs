{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Main (main) where

import Control.Monad
import Data.Configurant
import qualified Hedgehog
import qualified System.Environment as Env
import GHC.Generics (Generic)
import qualified Control.Exception as Exception
import GHC.Stack (withFrozenCallStack)
import Data.Either (isRight)
import Control.Monad.IO.Class (MonadIO)

data SampleE env = Sample
  { portNumber :: Var env "SAMPLE_PORT_NUMBER" Int,
    serviceName :: Var env "SAMPLE_SERVICE_NAME" String
  }
  deriving stock (Generic)
  deriving anyclass (Configurable)

throws :: (Hedgehog.MonadTest m, MonadIO m) => IO b -> m ()
throws x = withFrozenCallStack $ do
  res <- Hedgehog.evalIO (Exception.try @Exception.SomeException x)
  when (isRight res) $ do
    Hedgehog.footnote "Expected action to throw an error"
    Hedgehog.failure

throwsWhenVarsMissing :: Hedgehog.Property
throwsWhenVarsMissing = Hedgehog.property (throws (loadFromEnvironment @SampleE))

loadsWhenVarsPresent :: Hedgehog.Property
loadsWhenVarsPresent = Hedgehog.property (void (Hedgehog.evalIO (loadFromEnvironment @SampleE)))

main :: IO ()
main = do
  void (Hedgehog.check throwsWhenVarsMissing)

  Env.setEnv "SAMPLE_PORT_NUMBER" "6666"
  Env.setEnv "SAMPLE_SERVICE_NAME" "hi"

  void (Hedgehog.check loadsWhenVarsPresent)

  Env.setEnv "SAMPLE_PORT_NUMBER" "THIS IS NOT A PORT NUMBER"

  void (Hedgehog.check throwsWhenVarsMissing)
