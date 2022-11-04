# configurant

[![GitHub CI](https://github.com/patrickt/configurant/workflows/CI/badge.svg)](https://github.com/patrickt/configurant/actions)
[![Hackage](https://img.shields.io/hackage/v/configurant.svg?logo=haskell)](https://hackage.haskell.org/package/configurant)
[![Apache-2.0 license](https://img.shields.io/badge/license-Apache--2.0-blue.svg)](LICENSE)

> _figurant_, noun: an actor that figures in a scene without speaking or without taking a prominent part.

Configuring your application based on environment variables should be easy. `configurant`, a clone of Kelsey Hightower's [`envconfig`](https://github.com/kelseyhightower/envconfig), allows a user to populate a given data structure automatically by reading the environment variable specified in a given type. It aims to prioritize concision, interface simplicity, and error messages.

## Example

It's easier to show then tell. Given a `Server` type containing configuration parameters for some hypothetical web server:

``` haskell
{-# LANGUAGE DeriveGeneric, ImportQualifiedPost, OverloadedLabels, OverloadedStrings #-}

import Configurant (Config, (!))
import Configurant qualified as Config
import GHC.Generics (Generic)

data Server = Server
  { port :: Int,
    hostname :: String
  } deriving (Show, Generic)
```

you can construct a description of how to parse its environment variables, using the `OverloadedLabels` extension and `!` syntax from the [`named`](https://hackage.haskell.org/package/named) package:

``` haskell
serverEnv :: Config Server
serverEnv = Config.record
  ! #port (Config.read "SERVER_PORT")
  ! #hostname (Config.string "SERVER_HOSTNAME")
```

You can then use `Config.fromEnv` to construct a `Server` object based on the environment variables with which the program was invoked:

``` haskell
main :: IO ()
main = Config.fromEnv serverEnv >>= print
```

There exists the `Config.fromEnvOrExit` helper to handle the common case of printing errors to stdout and then exiting.

## How does this work?

In Go, it's trivial to add metadata to fields of a structure thanks to struct tags. Haskell doesn't support such a feature natively, but we can embed it in the type system without too much trouble. The `higgledy` package allows us to treat our `Server` type as though it were a higher-kinded type, of kind `(Type -> Type) -> Type`. In other words, this representation has a *shape functor* that wraps its fields, as though we had declared a type that looked like this:

``` haskell
data ServerF f = ServerF
  { port :: f Int
  , hostname :: f Int
  } deriving (Show, Generic)
```

With this higher-kinded representation, it's fairly trivial to slot in a validation applicative as the `f` parameter, which allows us to specify validators for individual fields. All the details of this higher-kinded representation are abstracted away in this library; if you use the `!` syntax and overloaded labels, you should never have to worry about it.

## Thanks

Kelsey Hightower's `envconfig` served as the primary source of inspiration. The idea to use `higgledy` came from me finding [`harg`](https://hackage.haskell.org/package/harg), though I didn't look at its implementation: `harg` does much more than `configurant` does but has a larger dependency footprint.
