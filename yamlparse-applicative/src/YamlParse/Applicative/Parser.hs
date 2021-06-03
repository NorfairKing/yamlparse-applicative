{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module YamlParse.Applicative.Parser where

import Control.Applicative
import Control.Monad
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Validity.Text ()
import Data.Vector (Vector)
import qualified Data.Yaml as Yaml
import Text.Read

-- | A parser that takes values of type 'i' as input and parses them into values of type 'o'
--
-- Note that there is no 'Monad' instance.
data Parser i o where
  -- | Return the input
  ParseAny :: Parser i i
  -- | Parse via an extra parsing function
  ParseExtra :: (o -> Yaml.Parser u) -> Parser i o -> Parser i u
  -- | Match an exact value
  ParseEq ::
    (Show o, Eq o) =>
    o ->
    -- | Shown version of the o in the previous argument
    Text ->
    Parser i o ->
    Parser i o
  -- | Parse 'null' only.
  ParseNull :: Parser Yaml.Value ()
  -- | Parse 'null' as 'Nothing' and the rest as 'Just'.
  ParseMaybe :: Parser Yaml.Value o -> Parser Yaml.Value (Maybe o)
  -- | Parse a boolean value
  ParseBool :: Maybe Text -> Parser Bool o -> Parser Yaml.Value o
  -- | Parse a String value
  ParseString ::
    -- | Extra info about what the string represents
    -- This info will be used during parsing for error messages and in the schema for documentation.
    Maybe Text ->
    Parser Text o ->
    Parser Yaml.Value o
  -- | Parse a numeric value
  ParseNumber ::
    -- | Extra info about what the number represents
    -- This info will be used during parsing for error messages and in the schema for documentation.
    Maybe Text ->
    Parser Scientific o ->
    Parser Yaml.Value o
  -- | Parse an array
  ParseArray ::
    -- | Extra info about what the array represents
    -- This info will be used during parsing for error messages and in the schema for documentation.
    Maybe Text ->
    Parser Yaml.Array o ->
    Parser Yaml.Value o
  -- | Parse an object
  ParseObject ::
    -- | Extra info about what the object represents
    -- This info will be used during parsing for error messages and in the schema for documentation.
    Maybe Text ->
    Parser Yaml.Object a ->
    Parser Yaml.Value a
  -- | Parse a list of elements all in the same way
  ParseList ::
    Parser Yaml.Value o ->
    Parser Yaml.Array (Vector o)
  -- | Parse a map where the keys are the yaml keys
  ParseMap ::
    Parser Yaml.Value v ->
    Parser Yaml.Object (HashMap Text v)
  -- | Parse a map's keys via a given parser
  ParseMapKeys ::
    Ord k =>
    Parser Text k ->
    Parser Yaml.Object (HashMap Text v) ->
    Parser Yaml.Object (Map k v) -- Once we get out of a HashMap, we'll want to stay out.

  -- | Parse a field of an object
  ParseField ::
    -- | The key of the field
    Text ->
    FieldParser o ->
    Parser Yaml.Object o
  -- | A pure value
  ParsePure :: a -> Parser i a
  -- | To implement Functor
  ParseFmap :: (a -> b) -> Parser i a -> Parser i b
  -- | To implement Applicative
  ParseAp :: Parser i (a -> b) -> Parser i a -> Parser i b
  -- | To implement Alternative
  ParseAlt :: [Parser i o] -> Parser i o
  -- | Add comments to the parser.
  -- This info will be used in the schema for documentation.
  ParseComment :: Text -> Parser i o -> Parser i o

instance Functor (Parser i) where
  fmap = ParseFmap

-- Realy only makes sense for 'Parser Yaml.Object', but we need 'Parser i' here to get the 'Alternative' instance to work
instance Applicative (Parser i) where
  pure = ParsePure
  (<*>) = ParseAp

instance Alternative (Parser i) where
  empty = ParseAlt []
  l <|> r = ParseAlt [l, r]
  some = undefined -- TODO figure out what to do here
  many = undefined

data FieldParser o where
  FieldParserFmap :: (a -> b) -> FieldParser a -> FieldParser b
  FieldParserRequired :: YamlParser o -> FieldParser o
  FieldParserOptional :: YamlParser o -> FieldParser (Maybe o)
  FieldParserOptionalWithDefault :: Show o => YamlParser o -> o -> FieldParser o

instance Functor FieldParser where
  fmap = FieldParserFmap

type YamlParser a = Parser Yaml.Value a

type ObjectParser a = Parser Yaml.Object a

type KeyParser a = Parser Text a

-- | Declare a parser of a named object
objectParser :: Text -> ObjectParser o -> YamlParser o
objectParser name = ParseObject (Just name)

-- | Declare a parser of an unnamed object
--
-- Prefer 'objectParser' if you can.
unnamedObjectParser :: ObjectParser o -> YamlParser o
unnamedObjectParser = ParseObject Nothing

-- | Parse a string-like thing by 'Read'-ing it
--
-- You probably don't want to use 'Read'.
viaRead :: Read a => YamlParser a
viaRead = maybeParser readMaybe $ T.unpack <$> ParseString Nothing ParseAny

-- | Declare a parser for an exact string.
--
-- You can use this to parse a constructor in an enum for example:
--
-- > data Fruit = Apple | Banana
-- >
-- > instance YamlSchema Fruit where
-- >   yamlSchema = Apple <$ literalString "Apple" <|> Banana <$ literalString "Banana"
literalString :: Text -> YamlParser Text
literalString t = ParseString Nothing $ ParseEq t (T.pack $ show t) ParseAny

-- | Declare a parser for a value using its show instance
--
-- Note that no value is read. The parsed string is just compared to the shown given value.
--
-- You can use this to parse a constructor in an enum when it has a 'Show' instance.
--
-- For example:
--
-- > data Fruit = Apple | Banana | Melon
-- >   deriving (Show, Eq)
-- >
-- > instance YamlSchema Fruit where
-- >   yamlSchema = alternatives
-- >      [ literalShowString Apple
-- >      , literalShowString Banana
-- >      , literalShowString Melon
-- >      ]
literalShowValue :: Show a => a -> YamlParser a
literalShowValue v = v <$ literalString (T.pack $ show v)

-- | Declare a parser for a value using its show instance
--
-- Note that no value is read. The parsed string is just compared to the shown given value.
--
-- You can use this to parse a constructor in an enum when it has a 'ToJSON' instance.
--
-- For example
--
-- > data Fruit = Apple | Banana | Melon
-- >   deriving (Eq, Generic)
-- >
-- > instance ToJSON Fruit
-- >
-- > instance YamlSchema Fruit where
-- >   yamlSchema = alternatives
-- >      [ literalValue Apple
-- >      , literalValue Banana
-- >      , literalValue Melon
-- >      ]
literalValue :: Yaml.ToJSON a => a -> YamlParser a
literalValue v = v <$ ParseEq (Yaml.toJSON v) (TE.decodeUtf8 $ LB.toStrict $ JSON.encode v) ParseAny

-- | Use the first parser of the given list that succeeds
--
-- You can use this to parse a constructor in an enum.
--
-- For example:
--
-- > data Fruit = Apple | Banana | Melon
-- >
-- > instance YamlSchema Fruit where
-- >   yamlSchema = alternatives
-- >      [ Apple <$ literalString "Apple"
-- >      , Banana <$ literalString "Banana"
-- >      , Melon <$ literalString "Melon"
-- >      ]
alternatives :: [Parser i o] -> Parser i o
alternatives = ParseAlt

-- | Add a comment to a parser
--
-- This info will be used in the schema for documentation.
--
-- For example:
--
-- > data Result = Error | Ok
-- > instance YamlSchema Result where
-- >   yamlSchema = alternatives
-- >     [ Error <$ literalString "Error" <?> "An error"
-- >     , Ok <$ literalString "Ok" <?> "Oll Klear"
-- >     ]
(<?>) :: Parser i a -> Text -> Parser i a
(<?>) = flip ParseComment

-- | Add a list of lines of comments to a parser
--
-- This info will be used in the schema for documentation.
--
-- For example:
--
-- > data Result = Error | Ok
-- > instance YamlSchema Result where
-- >   yamlSchema = alternatives
-- >     [ Error <$ literalString "Error" <??> ["Just an error", "but I've got a lot to say about this"]
-- >     , Ok <$ literalString "Ok" <??> ["Oll Klear", "I really don't know where 'OK' comes from?!"]
-- >     ]
(<??>) :: Parser i a -> [Text] -> Parser i a
(<??>) p ts = p <?> T.unlines ts

-- | A parser for a required field at a given key with a parser for what is found at that key
requiredFieldWith :: Text -> Text -> YamlParser a -> ObjectParser a
requiredFieldWith k h func = ParseComment h $ ParseField k $ FieldParserRequired func

-- | A parser for a required field at a given key with a parser for what is found at that key without a help text
requiredFieldWith' :: Text -> YamlParser a -> ObjectParser a
requiredFieldWith' k func = ParseField k $ FieldParserRequired func

-- | A parser for an optional field at a given key with a parser for what is found at that key
optionalFieldWith :: Text -> Text -> YamlParser a -> ObjectParser (Maybe a)
optionalFieldWith k h func = ParseComment h $ ParseField k $ fmap join $ FieldParserOptional $ ParseMaybe func

-- | A parser for an optional field at a given key with a parser for what is found at that key without a help text
optionalFieldWith' :: Text -> YamlParser a -> ObjectParser (Maybe a)
optionalFieldWith' k func = ParseField k $ fmap join $ FieldParserOptional $ ParseMaybe func

-- | A parser for an optional field at a given key with a default value and a parser for what is found at that key
--
-- For the sake of documentation, the default value needs to be showable.
optionalFieldWithDefaultWith :: Show a => Text -> a -> Text -> YamlParser a -> ObjectParser a
optionalFieldWithDefaultWith k d h func = ParseComment h $ ParseField k $ FieldParserOptionalWithDefault func d

-- | A parser for an optional field at a given key with a default value and a parser for what is found at that key without a help text
--
-- For the sake of documentation, the default value needs to be showable.
optionalFieldWithDefaultWith' :: Show a => Text -> a -> YamlParser a -> ObjectParser a
optionalFieldWithDefaultWith' k d func = ParseField k $ FieldParserOptionalWithDefault func d

-- | Make a parser that parses a value using the given extra parsing function
--
-- You can use this to make a parser for a type with a smart constructor.
-- Prefer 'eitherParser' if you can so you get better error messages.
--
-- Example:
--
-- > parseUsername :: Text -> Maybe Username
-- >
-- > instance YamlSchema Username where
-- >   yamlSchema = maybeParser parseUsername yamlSchema
maybeParser :: Show o => (o -> Maybe u) -> Parser i o -> Parser i u
maybeParser func = ParseExtra $ \o -> case func o of
  Nothing -> fail $ "Parsing of " <> show o <> " failed."
  Just u -> pure u

-- | Make a parser that parses a value using the given extra parsing function
--
-- You can use this to make a parser for a type with a smart constructor.
-- If you don't have a 'Show' instance for your 'o', then you can use 'extraParser' instead.
--
-- Example:
--
-- > parseUsername :: Text -> Either String Username
-- >
-- > instance YamlSchema Username where
-- >   yamlSchema = eitherParser parseUsername yamlSchema
eitherParser :: Show o => (o -> Either String u) -> Parser i o -> Parser i u
eitherParser func = ParseExtra $ \o -> case func o of
  Left err -> fail $ "Parsing of " <> show o <> " failed with error: " <> err <> "."
  Right u -> pure u

-- | Make a parser that parses a value using the given extra parsing function
--
-- You can use this to make a parser for a type with a smart constructor.
-- Prefer 'eitherParser' if you can, use this if you don't have a 'Show' instance for your 'o'.
extraParser :: (o -> Yaml.Parser u) -> Parser i o -> Parser i u
extraParser = ParseExtra
