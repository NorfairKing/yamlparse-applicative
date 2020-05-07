{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import YamlParse.Applicative

main :: IO ()
main =
  forM_
    (explainParser (yamlSchema :: YamlParser MyConfig))
    $ T.putStrLn . prettySchema

data MyConfig
  = MyConfig
      { myConfigText :: Text,
        myConfigScientific :: Maybe Scientific,
        myConfigList :: [Bool],
        myConfigSub :: Maybe MySubConfig,
        myConfigFruit :: Fruit
      }
  deriving (Show)

instance YamlSchema MyConfig where
  yamlSchema =
    object "MyConfig" $
      MyConfig
        <$> requiredField "foo" "My foo"
        <*> optionalField "bar" "My bar"
        <*> optionalFieldWithDefault "quux" [] "My quux"
        <*> optionalField "sub" "My sub"
        <*> requiredField "fruit" "My fruit"

data MySubConfig
  = MySubConfig
      { mySubConfigBool :: Maybe Bool,
        mySubConfigText :: Text,
        mySubConfigAlt :: Either Text Bool
      }
  deriving (Show)

instance YamlSchema MySubConfig where
  yamlSchema =
    object "MySubConfig" $
      MySubConfig
        <$> optionalField "foofoo" "My foofoo"
        <*> optionalFieldWithDefault "barbar" "defaultTextHere" "My bar"
        <*> (Left <$> (requiredField "left" "The left case") <|> Right <$> (requiredField "right" "The right case"))

data Fruit = Apple | Banana | Melon
  deriving (Show, Generic)

instance YamlSchema Fruit where
  yamlSchema =
    alternatives
      [ literalShowValue Apple,
        literalShowValue Banana,
        literalShowValue Melon
      ]
