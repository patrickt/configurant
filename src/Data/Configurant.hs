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

module Data.Configurant
  ( Var,
    Configurable (..),
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

data Style = Tag | Plain

-- | If we could stick role annotations on type families, we could
-- express this function without resorting to unsafeCoerce.
coerceStyle :: forall e (x :: Style) (y :: Style). () => e x -> e y
coerceStyle = unsafeCoerce

data ConfigError
  = MissingKey String
  | ReadError String String
  deriving stock (Eq, Show)

class Configurable cfg where
  loadFromEnvironment ::
    IO (Validation (NonEmpty ConfigError) (cfg 'Plain))
  default loadFromEnvironment ::
    (Generic (cfg 'Tag), GConfigurable (Rep (cfg 'Tag))) =>
    IO (Validation (NonEmpty ConfigError) (cfg 'Plain))
  loadFromEnvironment = fmap (fmap (coerceStyle @_ @'Tag @'Plain . to)) gloadFromEnvironment

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

type family Var (env :: Style) (tag :: Symbol) ty where
  Var 'Plain _ ty = ty
  Var 'Tag name ty = Tagged name ty

-- Var ty _ 'Plain = ty
-- Var ty name 'Tag = Tagged name ty
