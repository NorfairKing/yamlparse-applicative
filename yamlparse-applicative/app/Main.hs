{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text.IO as T
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
        myConfigSub :: Maybe MySubConfig
      }
  deriving (Show, Eq)

instance YamlSchema MyConfig where
  yamlSchema =
    object "MyConfig" $
      MyConfig
        <$> requiredField "foo" "My foo"
        <*> optionalField "bar" "My bar"
        <*> optionalFieldWithDefault "quux" [] "My quux"
        <*> optionalField "sub" "My sub"

data MySubConfig
  = MySubConfig
      { mySubConfigBool :: Maybe Bool,
        mySubConfigText :: Text,
        mySubConfigAlt :: Either Text Bool
      }
  deriving (Show, Eq)

instance YamlSchema MySubConfig where
  yamlSchema =
    object "MySubConfig" $
      MySubConfig
        <$> optionalField "foofoo" "My foofoo"
        <*> optionalFieldWithDefault "barbar" "defaultTextHere" "My bar"
        <*> (Left <$> (requiredField "left" "The left case") <|> Right <$> (requiredField "right" "The right case"))
