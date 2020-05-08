{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module YamlParse.Applicative.Class where

import qualified Data.Aeson as JSON
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.Scientific
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import Path
import YamlParse.Applicative.Implement
import YamlParse.Applicative.Parser

-- | A class of types for which a schema is defined.
--
-- Note that you do not have to use this class and can just use your own parser values.
-- Note also that the parsing of a type of this class should correspond to the parsing of the type in the FromJSON class.
class YamlSchema a where
  {-# MINIMAL yamlSchema #-}

  -- | A yamlschema for one value
  --
  -- See the sections on helper functions for implementing this for plenty of examples.
  yamlSchema :: YamlParser a

  -- | A yamlschema for a list of values
  --
  -- This is really only useful for cases like 'Char' and 'String'
  yamlSchemaList :: YamlParser [a]
  yamlSchemaList = V.toList <$> ParseArray Nothing (ParseList yamlSchema)

-- | A class of types for which a schema for keys is defined.
class YamlKeySchema a where
  yamlKeySchema :: KeyParser a

instance YamlSchema () where
  yamlSchema = pure ()

instance YamlSchema Bool where
  yamlSchema = ParseBool Nothing ParseAny

instance YamlSchema Char where
  yamlSchema =
    ParseString Nothing $
      maybeParser
        ( \cs -> case T.unpack cs of
            [] -> Nothing
            [c] -> Just c
            _ -> Nothing
        )
        ParseAny
  yamlSchemaList = T.unpack <$> yamlSchema

instance YamlSchema Text where
  yamlSchema = ParseString Nothing ParseAny

instance YamlKeySchema Text where
  yamlKeySchema = ParseAny

instance YamlKeySchema String where
  yamlKeySchema = T.unpack <$> yamlKeySchema

instance YamlSchema Scientific where
  yamlSchema = ParseNumber Nothing ParseAny

instance YamlSchema Int where
  yamlSchema = boundedIntegerSchema

instance YamlSchema Int8 where
  yamlSchema = boundedIntegerSchema

instance YamlSchema Int16 where
  yamlSchema = boundedIntegerSchema

instance YamlSchema Int32 where
  yamlSchema = boundedIntegerSchema

instance YamlSchema Int64 where
  yamlSchema = boundedIntegerSchema

instance YamlSchema Word where
  yamlSchema = boundedIntegerSchema

instance YamlSchema Word8 where
  yamlSchema = boundedIntegerSchema

instance YamlSchema Word16 where
  yamlSchema = boundedIntegerSchema

instance YamlSchema Word32 where
  yamlSchema = boundedIntegerSchema

instance YamlSchema Word64 where
  yamlSchema = boundedIntegerSchema

boundedIntegerSchema :: (Integral i, Bounded i) => YamlParser i
boundedIntegerSchema = maybeParser toBoundedInteger $ ParseNumber Nothing ParseAny

instance YamlSchema (Path Rel File) where
  yamlSchema = maybeParser parseRelFile yamlSchema

instance YamlSchema (Path Rel Dir) where
  yamlSchema = maybeParser parseRelDir yamlSchema

instance YamlSchema (Path Abs File) where
  yamlSchema = maybeParser parseAbsFile yamlSchema

instance YamlSchema (Path Abs Dir) where
  yamlSchema = maybeParser parseAbsDir yamlSchema

instance YamlSchema Yaml.Value where
  yamlSchema = ParseAny

instance YamlSchema a => YamlSchema (Maybe a) where
  yamlSchema = ParseMaybe yamlSchema

instance YamlSchema a => YamlSchema (Vector a) where
  yamlSchema = ParseArray Nothing (ParseList yamlSchema)

instance YamlSchema a => YamlSchema [a] where
  yamlSchema = yamlSchemaList

instance YamlSchema a => YamlSchema (NonEmpty a) where
  yamlSchema = extraParser go yamlSchema
    where
      go :: [a] -> Yaml.Parser (NonEmpty a)
      go as = case NE.nonEmpty as of
        Nothing -> fail "Nonempty list expected, but got an empty list"
        Just ne -> pure ne

instance (Ord a, YamlSchema a) => YamlSchema (Set a) where
  yamlSchema = S.fromList <$> yamlSchema

instance (Ord k, YamlKeySchema k, YamlSchema v) => YamlSchema (Map k v) where
  yamlSchema = ParseObject Nothing $ ParseMapKeys yamlKeySchema $ ParseMap yamlSchema

-- | There is no instance using YamlKeySchema k yet.
-- Ideally there wouldn't be one for HashMap Text either because it's insecure,
-- but the yaml arrives in a HashMap anyway so we might as well expose this.
instance YamlSchema v => YamlSchema (HashMap Text v) where
  yamlSchema = ParseObject Nothing $ ParseMap yamlSchema

-- | A parser for a required field in an object at a given key
requiredField :: YamlSchema a => Text -> Text -> ObjectParser a
requiredField k h = requiredFieldWith k h yamlSchema

-- | A parser for a required field in an object at a given key without a help text
requiredField' :: YamlSchema a => Text -> ObjectParser a
requiredField' k = requiredFieldWith' k yamlSchema

-- | A parser for an optional field in an object at a given key
optionalField :: YamlSchema a => Text -> Text -> ObjectParser (Maybe a)
optionalField k h = optionalFieldWith k h yamlSchema

-- | A parser for an optional field in an object at a given key without a help text
optionalField' :: YamlSchema a => Text -> ObjectParser (Maybe a)
optionalField' k = optionalFieldWith' k yamlSchema

-- | A parser for an optional field in an object at a given key with a default value
optionalFieldWithDefault :: (Show a, YamlSchema a) => Text -> a -> Text -> ObjectParser a
optionalFieldWithDefault k d h = optionalFieldWithDefaultWith k d h yamlSchema

-- | A parser for an optional field in an object at a given key with a default value without a help text
optionalFieldWithDefault' :: (Show a, YamlSchema a) => Text -> a -> ObjectParser a
optionalFieldWithDefault' k d = optionalFieldWithDefaultWith' k d yamlSchema

-- | Helper function to implement 'FromJSON' via 'YamlSchema'
--
-- Example:
--
-- > instance FromJSON Config where
-- >   parseJSON = viaYamlSchema
viaYamlSchema :: YamlSchema a => Yaml.Value -> Yaml.Parser a
viaYamlSchema = implementParser yamlSchema

-- | A helper newtype to parse a yaml value using the YamlSchema parser.
--
-- Example:
--
-- > case Data.Yaml.decodeEither' contents of
-- >   Left e -> die $ show e
-- >   Right (ViaYamlSchema res) -> print res
--
-- This only helps you when you really don't want to implement a 'FromJSON' instance.
-- See 'viaYamlSchema' if you do.
newtype ViaYamlSchema a = ViaYamlSchema a
  deriving (Show, Eq, Generic)

instance YamlSchema a => Yaml.FromJSON (ViaYamlSchema a) where
  parseJSON = fmap ViaYamlSchema . viaYamlSchema
