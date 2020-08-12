{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Configurant.Internal
  ( Var,
    Configurable (..),
    Configured,
    loadFromEnvironment',
    coerceStyle,
    GConfigurable (..),
    ConfigError (..),
    describeConfigError,
    ConfigException (..),
    Style (..),
  )
where

import Data.List.NonEmpty
import Data.Proxy
import Data.Tagged
import Data.Validation
import GHC.Generics
import GHC.TypeLits
import qualified System.Environment as Env
import Text.Read
import Unsafe.Coerce
import Control.Exception (Exception (..), throwIO)

data Style = Tag | Plain

-- | If we could stick role annotations on type families, we could
-- express this function without resorting to unsafeCoerce.
coerceStyle :: forall e (x :: Style) (y :: Style). () => e x -> e y
coerceStyle = unsafeCoerce

data ConfigError
  = MissingKey String
  | ReadError String String
  deriving stock (Eq, Show)

describeConfigError :: ConfigError -> String
describeConfigError = \case
  MissingKey str -> "Required environment variable '" <> str <> "' not present"
  ReadError key val -> "Value (" <> val <> ") associated with key '" <> key <> "could not be parsed"

newtype ConfigException = ConfigException (NonEmpty ConfigError)
  deriving stock (Eq, Show)

instance Exception ConfigException where
  displayException (ConfigException errs) =
    unlines ("Errors encountered loading configuration from environment" : rest)
    where
      rest = toList (fmap describeConfigError errs)

-- | The 'Configurable' class represents types that can be loaded with 'loadFromEnvironment'.
-- You do not implement this class directly, but instead derive it with @DeriveAnyClass@,
-- given a 'Generic' instance for your type in question:
--
-- @
--  data SampleE env = Sample
--    { portNumber :: Var env \"SAMPLE_PORT_NUMBER\" Int,
--      serviceName :: Var env \"SAMPLE_SERVICE_NAME\" String
--    }
--    deriving stock (Generic)
--    deriving anyclass (Configurable)
-- @

class Configurable cfg where
  -- | This function returns an IO action that, when invoked, will read a given 'Configurable'
  -- datum from the system environment, assigning each 'Var' field based on the name associated
  -- with that field.
  loadFromEnvironment ::
    IO (Validation (NonEmpty ConfigError) (Configured cfg))
  default loadFromEnvironment ::
    (Generic (cfg 'Tag), GConfigurable (Rep (cfg 'Tag))) =>
    IO (Validation (NonEmpty ConfigError) (Configured cfg))
  loadFromEnvironment = fmap (fmap (coerceStyle @_ @'Tag @'Plain . to)) gloadFromEnvironment

-- | The 'Configured' type resolves the @env@ parameter in a given
-- 'Configurable' type. You'll usually use this along with a type
-- alias, to give a more readable name to that type:
--
-- @
--  type Sample = Configured SampleE
-- @
type Configured a = a 'Plain

-- | As 'loadFromEnvironment', but throws a 'ConfigException' when errors are encountered.
loadFromEnvironment' :: Configurable cfg => IO (Configured cfg)
loadFromEnvironment' = loadFromEnvironment >>= \case
  Failure errors -> throwIO (ConfigException errors)
  Success result -> pure result

-- | The 'Var' type family associates a field's type with the
-- name of the environment variable from which the field should be populated.
-- A value of type @Var env tag result@ is resolved, with the 'Configured' type
-- synonym, to a value of type @result@, so field manipulations all take place
-- as though we were using plain old data types.
type family Var (env :: Style) (tag :: Symbol) ty where
  Var 'Plain _ ty = ty
  Var 'Tag name ty = Tagged name ty


class GConfigurable g where
  gloadFromEnvironment :: IO (Validation (NonEmpty ConfigError) (g a))

instance GConfigurable f => GConfigurable (M1 t meta f) where
  gloadFromEnvironment = fmap (fmap M1) gloadFromEnvironment

instance (GConfigurable f, GConfigurable g) => GConfigurable (f :*: g) where
  gloadFromEnvironment = do
    left <- gloadFromEnvironment
    right <- gloadFromEnvironment
    pure ((:*:) <$> left <*> right)

instance {-# OVERLAPPING #-} forall name r. (KnownSymbol name) => GConfigurable (K1 r (Tagged (name :: Symbol) String)) where
  gloadFromEnvironment = do
    let varName = symbolVal (Proxy @name)
    mEnvVar <- Env.lookupEnv varName
    pure $ case mEnvVar of
      Nothing -> Failure (pure (MissingKey varName))
      Just val -> pure (K1 (Tagged val))

instance forall name a r. (KnownSymbol name, Read a) => GConfigurable (K1 r (Tagged (name :: Symbol) a)) where
  gloadFromEnvironment = do
    let varName = symbolVal (Proxy @name)
    mEnvVar <- Env.lookupEnv varName
    pure $ case (mEnvVar, fmap readEither mEnvVar) of
      (Just found, Just (Left _)) -> Failure (pure (ReadError varName found))
      (Just _, Just (Right val)) -> pure (K1 (Tagged val))
      _ -> Failure (pure (MissingKey varName))
