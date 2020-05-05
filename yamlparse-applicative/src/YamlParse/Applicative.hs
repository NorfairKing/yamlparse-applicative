module YamlParse.Applicative
  ( someFunc,
  )
where

import qualified Data.JSON.Schema as JSON
import qualified Data.Yaml as Yaml

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Parser a = Parser

explainParser :: Parser a -> JSON.Schema
explainParser = undefined

implementParser :: Parser a -> (Yaml.Value -> Yaml.Parser a)
implementParser = undefined
