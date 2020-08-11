{- |
Copyright: (c) 2020 Patrick Thomson
SPDX-License-Identifier: Apache-2.0
Maintainer: Patrick Thomson <patrick.william.thomson@gmail.com>

Manage configuation data from environment variables.
-}

module Configurant
       ( someFunc
       ) where


someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
