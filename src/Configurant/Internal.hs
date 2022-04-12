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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

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
import System.Environment.Blank (getEnvironment)
import Text.Read (readMaybe)
import Prelude hiding (read)
import Prettyprinter qualified as Pretty
import Data.Foldable hiding (toList)
import Prettyprinter (pretty, (<+>))
import Data.Typeable
import Prettyprinter.Render.String
import Data.Maybe
import GHC.Exts (IsList (..))

-- | A 'Config' of a type variable @a@ is a view of a higher-kinded analog of @a@, with its
-- fields populated with 'Spec' values. In other words, given some plain old data type:
--
-- @
--   data Server = Server { port :: Int, hostname :: String } deriving (Typeable, Generic)
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
--  serverDesc = 'record'
--    ! #port (Config.'read' \"SERVER_PORT\")
--    ! #hostname \"SERVER_HOSTNAME\"
-- @
--
-- You can also use the 'Data.Generic.HKD.build' function, or you can create an empty config with
-- 'mempty' and update its fields with 'Data.Generic.HKD.position' and 'Data.Generic.HKD.field'.
--
-- Once you've constructed a 'Config' value, you can evaluate it with 'fromEnv' or 'fromPairs'.
type Config a = HKD a Spec


-- | Environment variables are keyed by strings.
type Key = String

-- | A 'Spec' describes the manner in which a particular environment variable is parsed.
-- You use smart constructors, like 'read', to build them. Because 'Spec' implements
-- 'Alternative', you can also embed constant values with 'pure', perform left-biased
-- choice with '<|>', and specify optional values with 'optional'.
data Spec a where
  Keyed :: Spec a -> Key -> Spec a
  String :: Spec String
  NonEmpty :: Spec String
  Read :: (Typeable a, Read a) => Spec a
  Validate :: Typeable a => (Key -> Maybe String -> Validation Errors a) -> Spec a
  Fail :: HasCallStack => Spec a
  Pure :: a -> Spec a
  Ap :: Spec (a -> b) -> Spec a -> Spec b
  Choice :: Spec a -> Spec a -> Spec a

instance Pretty.Pretty (Spec a) where
  pretty = \case
    Keyed s _ -> pretty s
    String -> "string value"
    NonEmpty -> "nonempty string value"
    Read -> Pretty.viaShow (typeRep (Proxy @a)) <+> "value"
    Validate _ -> "custom validator for" <+> Pretty.viaShow (typeRep (Proxy @a))
    Fail -> "failure"
    Pure _ -> "constant value"
    Ap _ _ -> "apply"
    Choice a b -> "choice" <+> Pretty.parens (pretty a) <+> Pretty.parens (pretty b)

instance Show (Spec a) where
  showsPrec _ = renderShowS . Pretty.layoutPretty Pretty.defaultLayoutOptions . pretty

instance Functor Spec where fmap = liftA

instance Applicative Spec where
  pure = Pure
  (<*>) = Ap

instance Alternative Spec where
  (<|>) = Choice
  empty = Fail

-- I'd like Functor, Applicative, and Alternative for Spec, but it makes the validator a little fraught

-- | You can use string literals for the common case of parsing unquoted string arguments.
instance IsString (Spec String) where fromString = string

-- | The Configurable class represents types that can be interpreted with 'fromEnv' or 'fromPairs'.
-- You do not need to declare instances for it: all you need to do is provide a 'Generic' instance
-- and you will be opted into this class automatically.
type Configurable a =  (FunctorB (HKD a), Construct (Validation Errors) a)

-- | Describes an argument that should be read as a literal string value. You can
-- use @OverloadedStrings@ and elide this call if you wish.
string :: Key -> Spec String
string = Keyed String

-- | As 'string', but disallowing blank or unkeyed values.
nonEmptyString :: Key -> Spec String
nonEmptyString = Keyed NonEmpty

-- | Describes an argument that should be parsed with 'Prelude.read'. The 'Typeable'
-- constraint is used to provide error messages.
read :: (Typeable a, Read a) => Key -> Spec a
read = Keyed Read

-- | The lowest-level specification process: given a key and a function returning a
-- 'Validation' applicative, the function is called with the key argument and the
-- corresponding value in the environment, if any.
validate :: Typeable a => Key -> (Key -> Maybe String -> Validation Errors a) -> Spec a
validate k f = Keyed (Validate f) k

-- | Provides a specifier for the common case of taking text and returning an 'Either' 'String'.
-- For example, if you have a config value that contains embedded JSON, you can parse it
-- with Aeson by passing
parsing :: (Typeable a, IsString s) => Key -> (s -> Either String a) -> Spec a
parsing k f = validate k $ \k' ms -> do
  case ms of
    Nothing -> die (NoValueForKey k' "TODO")
    Just a  -> case f (fromString a) of
      Left err -> die (ParseError k' err)
      Right x  -> pure x

-- | Helper for specifying default values for fields. Used infix, like so:
--
-- > string "MY_VAR" `orDefault` "default value"
orDefault :: Spec a -> a -> Spec a
s `orDefault` v = s <|> pure v

-- | Describes the possible errors that can be encountered during environment parsing.
data Error
  = NoValueForKey Key String
  | EmptyValueForKey Key
  | ValidationError String
  | Unkeyed String
  | BadFormat String String
  | ParseError Key String
  deriving stock (Show, Eq)

instance Pretty.Pretty Error where
  pretty = \case
    NoValueForKey k s ->
      "Missing value for key " <> Pretty.squotes (pretty k) <+> Pretty.parens ("expected " <> pretty s)
    ValidationError cs ->
      "Failed custom validation error. Call stack:" <> Pretty.line <> Pretty.viaShow cs
    BadFormat k v ->
      "Incorrect format for key " <> Pretty.squotes (pretty k) <> ":" <+> pretty v
    ParseError reason _ -> "Parse error: " <> Pretty.squotes (Pretty.viaShow reason)
    EmptyValueForKey k -> "Got empty value when looking up key " <> Pretty.squotes (Pretty.viaShow k)
    Unkeyed _s ->
      "Internal invariant violated: unkeyed Spec value"

-- | Errors accumulate into a nonempty list.
newtype Errors = Errors { getErrors :: NonEmpty Error }
  deriving newtype (Show, Eq, Semigroup)

instance IsList Errors where
  type Item Errors = Error
  fromList = Errors . fromList
  toList = toList . getErrors

instance Pretty.Pretty Errors where
  pretty = foldl' (\t e -> t <> Pretty.line' <> pretty e) mempty . getErrors

die :: Error -> Validation Errors a
die = Validation.Failure . Errors . pure

toValidation :: forall a. Spec a -> [(String, String)] -> Validation Errors a
toValidation s env = case s of
  Fail -> die (ValidationError (show callStack))
  Pure a -> pure a
  Ap f a -> toValidation f env <*> toValidation a env
  Choice a b -> case toValidation a env of
    Validation.Failure _ -> toValidation b env
    Validation.Success x -> pure x
  Keyed sub key -> case sub of
    Validate f -> f key (lookup key env)
    String -> lookup key env & maybe (die (NoValueForKey key (show sub))) pure
    NonEmpty -> case lookup key env of
      Nothing -> die (NoValueForKey key (show sub))
      Just "" -> die (EmptyValueForKey key)
      Just x  -> pure x
    Read -> case fmap readMaybe (lookup key env) of
      Nothing -> die (NoValueForKey key (show sub))
      Just Nothing -> die (BadFormat key (fromMaybe "" (lookup key env))) -- todo: report value
      Just (Just v) -> pure v
    other -> toValidation other env
  _ -> die (Unkeyed (show s))

-- | Reads in a 'Config' and constructs the associated value based on the key-value pairs specified
-- in the program's environment (as returned from 'getEnvironment').
--
-- The constraints in this instance head are satisfied iff the result type has a 'Generic' instance.
fromEnv :: (Configurable a) => Config a -> IO (Either Errors a)
fromEnv c = flip fromPairs c <$> getEnvironment

-- | As 'fromEnv', except reading from a provided association list of key-value pairs.
fromPairs :: Configurable a => [(String, String)] -> Config a -> Either Errors a
fromPairs e = Validation.toEither . construct . bmap (`toValidation` e)
