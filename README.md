# configurant

[![GitHub CI](https://github.com/patrickt/configurant/workflows/CI/badge.svg)](https://github.com/patrickt/configurant/actions)
[![Hackage](https://img.shields.io/hackage/v/configurant.svg?logo=haskell)](https://hackage.haskell.org/package/configurant)
[![Apache-2.0 license](https://img.shields.io/badge/license-Apache--2.0-blue.svg)](LICENSE)

> _figurant_, noun: an actor that figures in a scene without speaking or without taking a prominent part.

Configuring your application based on environment variables should be easy. `figurant`, a clone of Kelsey Hightower's [`envconfig`](https://github.com/kelseyhightower/envconfig), allows a user to populate a given data structure automatically by reading the environment variable specified in a given type.

It's easier to show then tell. Given this code, which is both verbose and unsafe (due to calling `read`):

``` haskell
import Data.Either
import System.Environment

data Sample = Sample
  { portNumber :: Int,
    serviceName :: String,
    timeoutDuration :: Double
  }

populateSample :: IO Sample
populateSample = do
  pn <- lookupEnv "SAMPLE_PORT_NUMBER" >>= maybe fail read
  sn <- lookupEnv "SAMPLE_SERVICE_NAME"
  td <- lookupEnv "SAMPLE_TIMEOUT_DURATION"
```

We can instead write

``` haskell
import Data.Configurant as Config

data SampleE env = Sample
  { portNumber :: Var env "SAMPLE_PORT_NUMBER" Int,
    serviceName :: Var env "SAMPLE_SERVICE_NAME" String
    timeoutDuration :: Var env "SAMPLE_TIMEOUT_DURATION" Double
  } deriving (Generic, Configurable)

type Sample = SampleE Plain

populateSample :: IO Sample
populateSample = Config.loadFromEnvironment
```
