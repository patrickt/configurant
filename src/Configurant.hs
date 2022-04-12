{-# LANGUAGE NoImplicitPrelude #-}

-- |
--
-- This module is intended to be imported qualified:
--
-- @
--   import Configurant (Config, (!))
--   import Configurant qualified as Config
-- @
module Configurant
  ( -- * Types
    Config,
    Spec,

    -- * Specifiers
    string,
    nonEmptyString,
    read,
    validate,
    orDefault,

    -- * Evaluators
    fromEnv,
    fromEnvOrExit,
    fromPairs,

    -- * Errors
    Error (..),
    Errors (..),

    -- * Re-exports
    module Data.Generic.HKD,
    module Named,
    module Control.Applicative,
  )
where

import Configurant.Internal
import Control.Applicative
import Data.Generic.HKD
import Named
