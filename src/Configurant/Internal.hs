{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Configurant.Internal
where

import Control.Applicative
import Data.Function
import Data.Generic.HKD
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString (..))
import Data.Validation (Validation)
import Data.Validation qualified as Validation
import GHC.Stack
import System.Environment (getEnvironment)
import Text.Read (readMaybe)
import Prelude hiding (read)
import Prettyprinter qualified as Pretty
import Data.Foldable
import Prettyprinter (pretty, (<+>))
import Data.Typeable
import Prettyprinter.Render.String
import Data.Maybe

-- | A 'Config' of a type variable @a@ is a view of a higher-kinded analog of @a@, with its
-- fields populated with 'Spec' values. In other words, given some plain old data type:
--
-- @
--   import Configurant (Config, (!))
--   import Configurant qualified as Config
--   data Server = Server { port :: Int, hostname :: String }
-- @
--
-- a 'Config' @Server@ is able to hold 'Spec' 'Int' and 'Spec' String values, which means
-- we can use a 'Config' @Server@ to describe how to parse a @Server@ from the environment
-- variables present on the system. You'll mostly never need to access or modify such data
-- types: the only time this is relevant is when constructing them.
--
-- The most concise way to build 'Config' values is to use the 'record' function in concert
-- with the @OverloadedLabels@ extension and the 'Named.!' syntax from the @named@ package:
--
-- @
--  serverDesc :: Config Server
--  serverDesc = 'record' ! #port (Config.'read' \"SERVER_PORT\") ! #hostname \"SERVER_HOSTNAME\"
-- @
--
-- Once you've constructed a 'Config' value, you can evaluate it with 'readEnv' or 'readConfig'.
type Config a = HKD a Spec


-- | Environment variables are keyed by strings.
type Key = String

-- | A 'Spec' describes the manner in which a particular environment variable is parsed.
-- You use smart constructors, like 'read', to build them.
data Spec a where
  Keyed :: Spec a -> Key -> Spec a
  String :: Spec String
  Read :: (Typeable a, Read a) => Spec a
  Validate :: Typeable a => (Key -> Maybe String -> Validation Errors a) -> Spec a
  Fail :: HasCallStack => Spec a

instance Pretty.Pretty (Spec a) where
  pretty = \case
    Keyed s _ -> pretty s
    String -> "string value"
    Read -> Pretty.viaShow (typeRep (Proxy @a)) <+> "value"
    Validate _ -> "custom validator for" <+> Pretty.viaShow (typeRep (Proxy @a))
    Fail -> "failure"

instance Show (Spec a) where
  showsPrec _ = renderShowS . Pretty.layoutPretty Pretty.defaultLayoutOptions . pretty

-- I'd like Functor, Applicative, and Alternative for Spec, but it makes the validator a little fraught

-- | You can use string literals for the common case of parsing unquoted string arguments.
instance IsString (Spec String) where fromString = string

-- | Describes an argument that should be read as a literal string value. You can
-- use @OverloadedStrings@ and elide this call if you wish.
string :: Key -> Spec String
string = Keyed String

-- | Describes an argument that should be parsed with 'Prelude.read'. The 'Typeable'
-- constraint is used to provide error messages.
read :: (Typeable a, Read a) => Key -> Spec a
read = Keyed Read

-- | The lowest-level specification process: given a key and a function returning a
-- 'Validation' applicative, the function is called with the key argument and the
-- corresponding value in the environment, if any.
validate :: Typeable a => Key -> (Key -> Maybe String -> Validation Errors a) -> Spec a
validate k f = Keyed (Validate f) k

-- | Describes the possible errors that can be encountered during environment parsing.
data Error
  = NoValueForKey Key String
  | ValidationError CallStack
  | Unkeyed String
  | BadFormat String String
  deriving stock (Show)

-- | Errors accumulate into a nonempty list.
type Errors = NonEmpty Error

describeErrors :: Errors -> Pretty.Doc a
describeErrors = foldl' (\t e -> t <> Pretty.line' <> go e) mempty
  where
    go :: Error -> Pretty.Doc a
    go = \case
      NoValueForKey k s ->
        "Missing value for key " <> Pretty.squotes (pretty k) <+> Pretty.parens ("expected " <> pretty s)
      ValidationError cs ->
        "Unknown validation error. Call stack:" <> Pretty.line <> Pretty.viaShow cs
      BadFormat k v ->
        "Incorrect format for key " <> Pretty.squotes (pretty k) <> ":" <+> pretty v
      Unkeyed _s ->
        "Internal invariant violated: unkeyed Spec value"

die :: Error -> Validation Errors a
die = Validation.Failure . pure

toValidation :: forall a. Spec a -> [(String, String)] -> Validation Errors a
toValidation s env = case s of
  Fail -> die (ValidationError callStack)
  Keyed sub key -> case sub of
    Validate f -> f key (lookup key env)
    String -> lookup key env & maybe (die (NoValueForKey key (show sub))) pure
    Read -> case fmap readMaybe (lookup key env) of
      Nothing -> die (NoValueForKey key (show sub))
      Just Nothing -> die (BadFormat key (fromMaybe "" (lookup key env))) -- todo: report value
      Just (Just v) -> pure v
    other -> toValidation other env
  _ -> die (Unkeyed (show s))

-- | Reads in a 'Config' and constructs the associated value based on the key-value pairs specified
-- in the program's environment (as returned from 'getEnvironment').
readEnv :: (FunctorB (HKD a), Construct (Validation Errors) a) => Config a -> IO (Either Errors a)
readEnv c = flip readConfig c <$> getEnvironment

-- | As 'readEnv', except reading from a provided association list of key-value pairs.
readConfig :: (FunctorB (HKD a), Construct (Validation Errors) a) => [(String, String)] -> Config a -> Either Errors a
readConfig e = Validation.toEither . construct . bmap (`toValidation` e)
