{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module YamlParse.Applicative.PrettySpec
  ( spec,
  )
where

import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.List.NonEmpty
import Data.Map (Map)
import Data.Scientific
import Data.Set (Set)
import Data.Text (Text)
import Data.Typeable
import Data.Vector (Vector)
import Data.Word
import Path
import Test.Syd
import Test.Syd.Validity.Utils
import YamlParse.Applicative.Class
import YamlParse.Applicative.Demo
import YamlParse.Applicative.Pretty

spec :: Spec
spec = do
  schemaGoldenSpec @()
  schemaGoldenSpec @Bool
  schemaGoldenSpec @Char
  schemaGoldenSpec @String
  schemaGoldenSpec @Text
  schemaGoldenSpec @Scientific
  schemaGoldenSpec @Int
  schemaGoldenSpec @Int8
  schemaGoldenSpec @Int16
  schemaGoldenSpec @Int32
  schemaGoldenSpec @Int64
  schemaGoldenSpec @Word
  schemaGoldenSpec @Word8
  schemaGoldenSpec @Word16
  schemaGoldenSpec @Word32
  schemaGoldenSpec @Word64
  schemaGoldenSpec @(Path Rel File)
  schemaGoldenSpec @(Path Rel Dir)
  schemaGoldenSpec @(Path Abs File)
  schemaGoldenSpec @(Path Abs Dir)
  schemaGoldenSpec @Value
  schemaGoldenSpec @(Maybe Bool)
  schemaGoldenSpec @(Vector Bool)
  schemaGoldenSpec @[Bool]
  schemaGoldenSpec @(NonEmpty Int)
  schemaGoldenSpec @(Set Word)
  schemaGoldenSpec @(Map Text Value)
  schemaGoldenSpec @(HashMap Text Value)
  describe "Demo" $
    schemaGoldenSpec @MyConfig

schemaGoldenSpec :: forall a. (Typeable a, YamlSchema a) => Spec
schemaGoldenSpec = do
  let name = nameOf @a
  describe name $ do
    it (unwords ["renders the colourless schema for", name, "the same way as before"]) $
      pureGoldenTextFile ("test_resources/" <> name <> ".schema") (prettySchemaDoc @a)
    it (unwords ["renders the colourful schema for", name, "the same way as before"]) $
      pureGoldenTextFile ("test_resources/" <> name <> "-coloured.schema") (prettyColourisedSchemaDoc @a)
