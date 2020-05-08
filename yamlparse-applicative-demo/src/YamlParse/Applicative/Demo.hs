{-# LANGUAGE OverloadedStrings #-}

module YamlParse.Applicative.Demo where

import Control.Applicative
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text.IO as T
import YamlParse.Applicative

main :: IO ()
main =
  T.putStrLn . prettySchema $
    explainParser (yamlSchema :: YamlParser MyConfig)

data MyConfig
  = MyConfig
      { myConfigText :: Text,
        myConfigScientific :: Maybe Int,
        myConfigList :: [Bool],
        myConfigSub :: Maybe MySubConfig,
        myConfigFruit :: Fruit,
        myConfigNull :: (),
        myConfigMaybe :: Maybe Int,
        myConfigMap :: Map Text Int
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
        <*> requiredField "map" "My map"

data MySubConfig
  = MySubConfig
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
  deriving (Show)

instance YamlSchema Fruit where
  yamlSchema =
    alternatives
      [ literalShowValue Apple,
        literalShowValue Banana,
        literalShowValue Melon
      ]
