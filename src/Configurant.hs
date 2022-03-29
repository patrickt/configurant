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

    -- * Evaluators
    readEnv,
    readConfig,

    -- * Errors
    Error (..),
    Errors,

    -- * Re-exports
    module Data.Generic.HKD,
    module Named,
  )
where

import Configurant.Internal
import Data.Generic.HKD
import Named
