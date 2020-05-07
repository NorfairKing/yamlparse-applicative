{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module YamlParse.Applicative.Class where

import qualified Data.Aeson as JSON
import Data.Scientific
import qualified Data.Text as T
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
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

instance YamlSchema Bool where
  yamlSchema = ParseBool Nothing ParseAny

instance YamlSchema Char where
  yamlSchema =
    ParseString Nothing $
      ParseMaybe
        ( \cs -> case T.unpack cs of
            [] -> Nothing
            [c] -> Just c
            _ -> Nothing
        )
        ParseAny
  yamlSchemaList = T.unpack <$> yamlSchema

instance YamlSchema Text where
  yamlSchema = ParseString Nothing ParseAny

instance YamlSchema Scientific where
  yamlSchema = ParseNumber Nothing ParseAny

instance YamlSchema Yaml.Object where
  yamlSchema = ParseObject Nothing ParseAny

instance YamlSchema Yaml.Value where
  yamlSchema = ParseAny

instance YamlSchema a => YamlSchema (Vector a) where
  yamlSchema = ParseArray Nothing (ParseList yamlSchema)

instance YamlSchema a => YamlSchema [a] where
  yamlSchema = yamlSchemaList

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
