{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module YamlParse.Applicative.Demo where

import Control.Applicative
import Data.Aeson
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import Path
import YamlParse.Applicative

main :: IO ()
main =
  T.putStrLn . prettyColourisedSchema $
    explainParser (yamlSchema :: YamlParser MyConfig)

data MyConfig = MyConfig
  { myConfigText :: Text,
    myConfigScientific :: Maybe Int,
    myConfigList :: [Bool],
    myConfigSub :: Maybe MySubConfig,
    myConfigFruit :: Fruit,
    myConfigNull :: (),
    myConfigMaybe :: Maybe Int,
    myConfigMaybe2 :: Maybe Int,
    myConfigMap :: Map Text Int,
    myConfigSet :: Set Text,
    myConfigNEList :: NonEmpty Int,
    myConfigPath :: Path Rel File,
    myConfigNotNull :: Maybe Int
  }
  deriving (Show)

instance YamlSchema MyConfig where
  yamlSchema =
    objectParser "MyConfig" $
      MyConfig
        <$> requiredField "foo" "My foo"
        <*> optionalField "bar" "My bar"
        <*> optionalFieldWithDefault "quux" [] "My quux"
        <*> optionalField "sub" "My sub"
        <*> requiredField "fruit" "My fruit"
        <*> requiredField "empty" "My null"
        <*> requiredField "num" "My maybe"
        <*> optionalField "num2" "My maybe 2"
        <*> requiredField "map" "My map"
        <*> requiredField "set" "My set"
        <*> requiredField "nelist" "My nonempty list"
        <*> requiredField "path" "My path"
        <*> ParseField "notnull" (FieldParserOptional yamlSchema)

data MySubConfig = MySubConfig
  { mySubConfigBool :: Maybe Bool,
    mySubConfigText :: Text,
    mySubConfigAlt :: Either Text Bool
  }
  deriving (Show)

instance YamlSchema MySubConfig where
  yamlSchema =
    objectParser "MySubConfig" $
      MySubConfig
        <$> optionalField "foofoo" "My foofoo"
        <*> optionalFieldWithDefault "barbar" "defaultTextHere" "My bar"
        <*> (Left <$> requiredField "left" "The left case" <|> Right <$> requiredField "right" "The right case")

data Fruit = Apple | Banana | Melon
  deriving (Show, Generic)

instance YamlSchema Fruit where
  yamlSchema =
    alternatives
      [ literalShowValue Apple,
        literalShowValue Banana,
        literalShowValue Melon
      ]

instance ToJSON Fruit
