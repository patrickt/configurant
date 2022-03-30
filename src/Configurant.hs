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
    read,
    validate,
    orDefault,

    -- * Evaluators
    fromEnv,
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
