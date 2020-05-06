{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module YamlParse.Applicative where

-- import Data.HashMap.Strict (HashMap)
-- import qualified Data.HashMap.Strict as HM

import Control.Monad
import Data.Maybe
import Data.Scientific
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml

someFunc :: IO ()
someFunc = do
  putStrLn "MyThing"
  forM_ (explainParser (fromYamlSchema :: Parser MyThing)) $
    T.putStrLn
      . prettySchema
  putStrLn ""
  putStrLn "MyConfig"
  forM_
    (explainParser (fromYamlSchema :: Parser MyConfig))
    $ T.putStrLn . prettySchema
  putStrLn ""

data MyConfig
  = MyConfig {myConfigText :: Text, myConfigInt :: Maybe Int, myConfigList :: [Bool]}
  deriving (Show, Eq)

instance FromYamlSchema MyConfig where
  fromYamlSchema = MyConfig <$> requiredField "myText" <*> optionalField "myInt" <*> optionalFieldWithDefault "list" []

data MyThing = MyThing [Bool] Text

instance FromYamlSchema MyThing where
  fromYamlSchema = MyThing <$> "myBools" .: (V.toList <$> (ParseArray $ ParseBool <?> "true means open")) <*> "myString" .: (ParseString <?> "some string")

class FromYamlSchema a where
  fromYamlSchema :: Parser a

instance FromYamlSchema Bool where
  fromYamlSchema = ParseBool

instance FromYamlSchema Text where
  fromYamlSchema = ParseString

instance FromYamlSchema Int where
  fromYamlSchema = (fromMaybe (error "floating or will cause over/under flow") . Data.Scientific.toBoundedInteger) <$> ParseNumber

instance FromYamlSchema a => FromYamlSchema [a] where
  fromYamlSchema = V.toList <$> ParseArray fromYamlSchema

requiredField :: FromYamlSchema a => Text -> Parser a
requiredField k = ParseField k $ FieldParserRequired fromYamlSchema

optionalField :: FromYamlSchema a => Text -> Parser (Maybe a)
optionalField k = ParseField k $ FieldParserOptional fromYamlSchema

optionalFieldWithDefault :: (Show a, FromYamlSchema a) => Text -> a -> Parser a
optionalFieldWithDefault k d = ParseField k $ FieldParserOptionalWithDefault fromYamlSchema d

data Parser a where
  ParseAny :: Parser Yaml.Value
  ParseBool :: Parser Bool
  ParseString :: Parser Text
  ParseNumber :: Parser Scientific
  ParseArray :: Parser a -> Parser (Vector a)
  ParseField :: Text -> FieldParser a -> Parser a
  -- ParseObject :: HashMap Text (FieldParser a) -> Parser (HashMap Text a)
  ParsePure :: a -> Parser a
  ParseFmap :: (a -> b) -> Parser a -> Parser b
  ParseAp :: Parser (a -> b) -> Parser a -> Parser b
  ParseComment :: Text -> Parser a -> Parser a

instance Functor Parser where
  fmap = ParseFmap

instance Applicative Parser where
  pure = ParsePure
  (<*>) = ParseAp

data FieldParser a where
  FieldParserRequired :: Parser a -> FieldParser a
  FieldParserOptional :: Parser a -> FieldParser (Maybe a)
  FieldParserOptionalWithDefault :: Show a => Parser a -> a -> FieldParser a

(<?>) :: Parser a -> Text -> Parser a
(<?>) = flip ParseComment

(.:) :: Text -> Parser a -> Parser a
(.:) k p = ParseField k $ FieldParserRequired p

implementParser :: Parser a -> (Yaml.Value -> Yaml.Parser a)
implementParser = go
  where
    go :: Parser a -> (Yaml.Value -> Yaml.Parser a)
    go = \case
      -- TODO figure out which text to put in the 'with' things.
      ParseAny -> pure
      ParseBool -> Yaml.withBool "Bool" pure
      ParseString -> Yaml.withText "Text" pure
      ParseNumber -> Yaml.withScientific "Number" pure
      ParseArray p -> Yaml.withArray "Array" $ V.mapM (go p)
      ParseField k fp -> Yaml.withObject "Object" $ goField k fp
      ParsePure v -> const $ pure v
      ParseAp pf p -> \v -> go pf v <*> go p v
      ParseFmap f p -> fmap f . go p
      ParseComment _ p -> go p
    -- goField :: Text -> FieldParser a -> Yaml.Object -> Yaml.Parser a
    goField k fp o = case fp of
      FieldParserRequired p -> o Yaml..: k >>= go p
      FieldParserOptional p -> do
        mv <- o Yaml..:? k
        case mv of
          Nothing -> pure Nothing
          Just v -> Just <$> go p v
      FieldParserOptionalWithDefault p d -> do
        mv <- o Yaml..:? k
        case mv of
          Nothing -> pure d
          Just v -> go p v

-- FieldParserOptional p -> o Yaml..:? k >>= go p

-- Nothing means that nothing even needs to be parsed,
-- you just get the 'a' without parsing anything.
-- This is for the 'pure' case.
explainParser :: Parser a -> Maybe Schema
explainParser = go
  where
    go :: Parser a -> Maybe Schema
    go = \case
      ParseAny -> Just AnySchema
      ParseBool -> Just BoolSchema
      ParseNumber -> Just NumberSchema
      ParseString -> Just StringSchema
      ParseArray p -> ArraySchema <$> go p
      ParseField k fp -> case fp of
        FieldParserRequired p -> FieldSchema k True Nothing <$> go p
        FieldParserOptional p -> FieldSchema k False Nothing <$> go p
        FieldParserOptionalWithDefault p d -> FieldSchema k False (Just $ T.pack $ show d) <$> go p
      ParsePure _ -> Nothing
      ParseFmap _ p -> go p
      ParseAp pf p -> ApSchema <$> go pf <*> go p
      ParseComment t p -> CommentSchema t <$> go p

-- TODO make schema a gadt?
data Schema
  = AnySchema
  | BoolSchema
  | NumberSchema
  | StringSchema
  | ArraySchema Schema
  | FieldSchema Text Bool (Maybe Text) Schema
  | ApSchema Schema Schema -- We'll take this to mean 'and'
  | CommentSchema Text Schema
  deriving (Show, Eq)

prettySchema :: Schema -> Text
prettySchema = renderStrict . layoutPretty defaultLayoutOptions . schemaDoc

schemaDoc :: Schema -> Doc a
schemaDoc = go
  where
    go :: Schema -> Doc a
    go = \case
      AnySchema -> "<any>"
      BoolSchema -> "<bool>"
      NumberSchema -> "<number>"
      StringSchema -> "<string>"
      ArraySchema s -> "-" <+> go s
      FieldSchema k r d s ->
        let comments =
              align $ vsep $ map ("# " <>) $
                catMaybes
                  [ Just $
                      if r
                        then "required"
                        else "optional",
                    (("default: " <>) . pretty)
                      <$> d
                  ]
         in vsep [(pretty k <> ":"), indent 2 (go s <+> comments)]
      ApSchema s1 s2 -> align $ vsep [go s1, go s2]
      CommentSchema t s -> align $ vsep ["# " <> pretty t, go s]
