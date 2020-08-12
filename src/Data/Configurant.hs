-- |
-- Module:      Data.Configurant
-- Copyright:   (c) 2020 Patrick Thomson
-- License:     Apache-2.0
-- Maintainer:  Patrick Thomson <patrick.william.thomson@gmail.com>
-- Stability:   experimental
-- Portability: non-portable
-- Language:    Haskell2010
--
-- An interface for fluently loading configuration data from environment variables.
module Data.Configurant
  ( Configurable (..),
    Configured,
    loadFromEnvironment',
    Var,
    ConfigError(..),
    ConfigException(..),
  )
where

import Data.Configurant.Internal
