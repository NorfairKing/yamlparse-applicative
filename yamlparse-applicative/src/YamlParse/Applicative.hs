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
  putStrLn "MyConfig"
  forM_
    (explainParser (fromYamlSchema :: YamlParser MyConfig))
    $ T.putStrLn . prettySchema
  putStrLn ""

data MyConfig
  = MyConfig {myConfigText :: Text, myConfigScientific :: Maybe Scientific, myConfigList :: [Bool], myConfigSub :: Maybe MySubConfig}
  deriving (Show, Eq)

instance FromYamlSchema MyConfig where
  fromYamlSchema = ParseObject "MyConfig" $ MyConfig <$> requiredField "foo" <*> optionalField "bar" <*> optionalFieldWithDefault "quux" [] <*> optionalField "sub"

data MySubConfig
  = MySubConfig
      { mySubConfigBool :: Maybe Bool,
        mySubConfigText :: Text
      }
  deriving (Show, Eq)

instance FromYamlSchema MySubConfig where
  fromYamlSchema = ParseObject "MySubConfig" $ MySubConfig <$> optionalField "foofoo" <*> requiredField "barbar"

-- TODO split this up into FromYamlSchema and FromObjectSchema?
-- Probably better not because we want the contents of ParseObject
class FromYamlSchema a where
  fromYamlSchema :: YamlParser a

instance FromYamlSchema Bool where
  fromYamlSchema = ParseBool "Bool" ParseAny

instance FromYamlSchema Text where
  fromYamlSchema = ParseString "Text" ParseAny

instance FromYamlSchema Scientific where
  fromYamlSchema = ParseNumber "Scientific" ParseAny

instance FromYamlSchema Yaml.Array where
  fromYamlSchema = ParseArray "Array" ParseAny

instance FromYamlSchema Yaml.Object where
  fromYamlSchema = ParseObject "Object" ParseAny

instance FromYamlSchema Yaml.Value where
  fromYamlSchema = ParseAny

instance FromYamlSchema a => FromYamlSchema (Vector a) where
  fromYamlSchema = ParseArray "Vector" (ParseList fromYamlSchema)

instance FromYamlSchema a => FromYamlSchema [a] where
  fromYamlSchema = V.toList <$> ParseArray "List" (ParseList fromYamlSchema)

requiredField :: FromYamlSchema a => Text -> ObjectParser a
requiredField k = ParseField k $ FieldParserRequired fromYamlSchema

optionalField :: FromYamlSchema a => Text -> ObjectParser (Maybe a)
optionalField k = ParseField k $ FieldParserOptional fromYamlSchema

optionalFieldWithDefault :: (Show a, FromYamlSchema a) => Text -> a -> ObjectParser a
optionalFieldWithDefault k d = ParseField k $ FieldParserOptionalWithDefault fromYamlSchema d

type YamlParser a = Parser Yaml.Value a

type ObjectParser a = Parser Yaml.Object a

data Parser i o where
  ParseAny :: Parser i i
  ParseBool :: Text -> Parser Bool o -> Parser Yaml.Value o
  ParseString :: Text -> Parser Text o -> Parser Yaml.Value o
  ParseNumber :: Text -> Parser Scientific o -> Parser Yaml.Value o
  ParseList :: Parser Yaml.Value o -> Parser Yaml.Array (Vector o)
  ParseArray :: Text -> Parser Yaml.Array o -> Parser Yaml.Value o
  ParseField :: Text -> FieldParser o -> Parser Yaml.Object o
  ParseObject :: Text -> Parser Yaml.Object a -> Parser Yaml.Value a
  ParsePure :: a -> Parser i a
  ParseFmap :: (a -> b) -> Parser i a -> Parser i b
  ParseAp :: Parser i (a -> b) -> Parser i a -> Parser i b
  ParseComment :: Text -> Parser i o -> Parser i o

instance Functor (Parser i) where
  fmap = ParseFmap

instance Applicative (Parser Yaml.Object) where
  pure = ParsePure
  (<*>) = ParseAp

data FieldParser o where
  FieldParserRequired :: YamlParser o -> FieldParser o
  FieldParserOptional :: YamlParser o -> FieldParser (Maybe o)
  FieldParserOptionalWithDefault :: Show o => YamlParser o -> o -> FieldParser o

(<?>) :: Parser i a -> Text -> Parser i a
(<?>) = flip ParseComment

implementParser :: Parser i o -> (i -> Yaml.Parser o)
implementParser = go
  where
    go :: Parser i o -> (i -> Yaml.Parser o)
    go = \case
      -- TODO figure out which text to put in the 'with' things.
      ParseAny -> pure
      ParseBool t p -> Yaml.withBool (T.unpack t) $ go p
      ParseString t p -> Yaml.withText (T.unpack t) $ go p
      ParseNumber t p -> Yaml.withScientific (T.unpack t) $ go p
      ParseList p -> \l -> forM l $ \v -> go p v
      ParseArray t p -> Yaml.withArray (T.unpack t) $ go p
      ParseField key fp -> \o -> case fp of
        FieldParserRequired p -> o Yaml..: key >>= go p
        FieldParserOptional p -> do
          mv <- o Yaml..:? key
          case mv of
            Nothing -> pure Nothing
            Just v -> Just <$> go p v
        FieldParserOptionalWithDefault p d -> do
          mv <- o Yaml..:? key
          case mv of
            Nothing -> pure d
            Just v -> go p v
      ParseObject t p -> Yaml.withObject (T.unpack t) $ go p
      ParsePure v -> const $ pure v
      ParseAp pf p -> \v -> go pf v <*> go p v
      ParseFmap f p -> fmap f . go p
      ParseComment _ p -> go p

-- FieldParserOptional p -> o Yaml..:? k >>= go p

-- Nothing means that nothing even needs to be parsed,
-- you just get the 'a' without parsing anything.
-- This is for the 'pure' case.
explainParser :: Parser i o -> Maybe Schema
explainParser = go
  where
    go :: Parser i o -> Maybe Schema
    go = \case
      ParseAny -> Just AnySchema
      ParseBool t _ -> Just $ BoolSchema t
      ParseNumber t _ -> Just $ NumberSchema t
      ParseString t _ -> Just $ StringSchema t
      ParseArray t p -> ArraySchema t <$> go p
      ParseList p -> ListSchema <$> go p
      ParseField k fp -> case fp of
        FieldParserRequired p -> FieldSchema k True Nothing <$> go p
        FieldParserOptional p -> FieldSchema k False Nothing <$> go p
        FieldParserOptionalWithDefault p d -> FieldSchema k False (Just $ T.pack $ show d) <$> go p
      ParseObject t p -> ObjectSchema t <$> go p
      ParsePure _ -> Nothing
      ParseFmap _ p -> go p
      ParseAp pf p -> ApSchema <$> go pf <*> go p
      ParseComment t p -> CommentSchema t <$> go p

-- TODO make schema a gadt?
data Schema
  = AnySchema
  | BoolSchema Text
  | NumberSchema Text
  | StringSchema Text
  | ListSchema Schema
  | ArraySchema Text Schema
  | FieldSchema Text Bool (Maybe Text) Schema
  | ObjectSchema Text Schema
  | ApSchema Schema Schema -- We'll take this to mean 'and'
  | CommentSchema Text Schema
  deriving (Show, Eq)

prettySchema :: Schema -> Text
prettySchema = renderStrict . layoutPretty defaultLayoutOptions . schemaDoc

newtype Comments = Comments {commentsList :: [Doc ()]}
  deriving (Show)

emptyComments :: Comments
emptyComments = Comments []

comment :: Text -> Comments
comment t = Comments [pretty t]

instance Semigroup Comments where
  (Comments l1) <> (Comments l2) = Comments $ l1 <> l2

instance Monoid Comments where
  mempty = emptyComments
  mappend = (<>)

schemaDoc :: Schema -> Doc ()
schemaDoc = go emptyComments
  where
    go :: Comments -> Schema -> Doc ()
    go cs =
      let g = go cs
          mkComment :: Doc () -> Doc ()
          mkComment = ("# " <>)
          mkCommentsMDoc :: Comments -> Maybe (Doc ())
          mkCommentsMDoc = \case
            Comments [] -> Nothing
            Comments l -> Just $ align $ vsep $ map mkComment l
          e :: Doc () -> Comments -> Doc ()
          e s cs' =
            case mkCommentsMDoc cs' of
              Nothing -> s
              Just cd -> vsep [cd, s]
       in \case
            AnySchema -> e "<any>" cs
            BoolSchema t -> e "<bool>" $ cs <> comment t
            NumberSchema t -> e "<number>" $ cs <> comment t
            StringSchema t -> e "<string>" $ cs <> comment t
            ListSchema s -> g s
            ArraySchema t s -> "-" <+> align (go (cs <> comment t) s)
            ObjectSchema _ s -> g s
            FieldSchema k r md s ->
              let keyDoc :: Doc a
                  keyDoc = pretty k
                  requiredDoc :: Doc a
                  requiredDoc = if r then "required" else "optional"
                  mDefaultDoc :: Maybe (Doc a)
                  mDefaultDoc = (("default: " <>) . pretty) <$> md
                  cs' = cs <> (Comments (catMaybes [mDefaultDoc]))
               in vsep
                    [ keyDoc <> ":" <+> mkComment requiredDoc,
                      indent 2 $ e (g s) cs'
                    ]
            ApSchema s1 s2 -> align $ vsep [g s1, g s2]
            CommentSchema t s -> go (cs <> comment t) s
