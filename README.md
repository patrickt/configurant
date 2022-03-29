# configurant

[![GitHub CI](https://github.com/patrickt/configurant/workflows/CI/badge.svg)](https://github.com/patrickt/configurant/actions)
[![Hackage](https://img.shields.io/hackage/v/configurant.svg?logo=haskell)](https://hackage.haskell.org/package/configurant)
[![Apache-2.0 license](https://img.shields.io/badge/license-Apache--2.0-blue.svg)](LICENSE)

> _figurant_, noun: an actor that figures in a scene without speaking or without taking a prominent part.

Configuring your application based on environment variables should be easy. `configurant`, a clone of Kelsey Hightower's [`envconfig`](https://github.com/kelseyhightower/envconfig), allows a user to populate a given data structure automatically by reading the environment variable specified in a given type.

It's easier to show then tell. Given a `Server` type containing configuration parameters for some hypothetical web server:

``` haskell
{-# LANGUAGE ImportQualifiedPost, OverloadedLabels, OverloadedStrings #-}

import Configurant (Config, (!))
import Configurant qualified as Config

data Server = Server
  { port :: Int,
    hostname :: Int
  }
```

you can construct a description of how to parse its environment variables, using the `OverloadedLabels` extension and `!` syntax from the [`named`](https://hackage.haskell.org/package/named) package

``` haskell
serverEnv :: Config Server
serverEnv = Config.record
  ! #port (Config.read "SERVER_PORT")
  ! #hostname "SERVER_HOSTNAME"
```

You can then use `Config.fromEnv` to construct a `Server` object based on the environment variables with which the program was invoked:

``` haskell
main :: IO ()
main = Config.fromEnv serverEnv >>= print
```

Under the hood, this uses higher-kinded data types via the `higgledy` and `barbies` library, and the validation monad from `validators`.
