{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module YamlParse.Applicative
  ( module YamlParse.Applicative,
    module YamlParse.Applicative.Parser,
  )
where

import Control.Applicative
import Control.Monad
import qualified Data.Aeson as JSON
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Data.Scientific
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Validity
import Data.Validity.Text ()
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import qualified Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse
import Path
import Path.IO
import System.Exit
import YamlParse.Applicative.Parser

-- | A class of types for which a schema is defined.
--
-- Note that you do not have to use this class and can just use your own parser values.
-- Note also that the parsing of a type of this class should correspond to the parsing of the type in the FromJSON class.
class YamlSchema a where
  {-# MINIMAL yamlSchema #-}
  yamlSchema :: YamlParser a
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

-- | A parser for a required field at a given key
requiredField :: YamlSchema a => Text -> Text -> ObjectParser a
requiredField k h = requiredFieldWith k h yamlSchema

-- | A parser for a required field at a given key without a help text
requiredField' :: YamlSchema a => Text -> ObjectParser a
requiredField' k = requiredFieldWith' k yamlSchema

-- | A parser for an optional field at a given key
optionalField :: YamlSchema a => Text -> Text -> ObjectParser (Maybe a)
optionalField k h = optionalFieldWith k h yamlSchema

-- | A parser for an optional field at a given key without a help text
optionalField' :: YamlSchema a => Text -> ObjectParser (Maybe a)
optionalField' k = optionalFieldWith' k yamlSchema

-- | A parser for an optional field at a given key with a default value
optionalFieldWithDefault :: (Show a, YamlSchema a) => Text -> a -> Text -> ObjectParser a
optionalFieldWithDefault k d h = optionalFieldWithDefaultWith k d h yamlSchema

-- | A parser for an optional field at a given key with a default value without a help text
optionalFieldWithDefault' :: (Show a, YamlSchema a) => Text -> a -> ObjectParser a
optionalFieldWithDefault' k d = optionalFieldWithDefaultWith' k d yamlSchema

-- | Use a 'Parser' to parse a value from Yaml.
--
-- A 'Parser i o' corresponds exactly to a 'i -> Yaml.Parser o' and this function servers as evidence for that.
implementParser :: Parser i o -> (i -> Yaml.Parser o)
implementParser = go
  where
    go :: Parser i o -> (i -> Yaml.Parser o)
    go = \case
      ParseAny -> pure
      ParseEq v t p -> \i -> do
        r <- go p i
        if r == v then pure r else fail $ "Expected " <> T.unpack t <> " exactly but got: " <> show r
      ParseMaybe mf p -> \i -> do
        o <- go p i
        case mf o of
          Nothing -> fail "Parsing failed"
          Just u -> pure u
      -- We can't just do 'withBool (maybe "Bool" T.unpack mt)' because then there is an extra context in the error message.
      ParseBool mt p -> case mt of
        Just t -> Yaml.withBool (T.unpack t) $ go p
        Nothing -> \v -> case v of
          Yaml.Bool o -> go p o
          _ -> Aeson.typeMismatch "Bool" v
      ParseString mt p -> case mt of
        Just t -> Yaml.withText (T.unpack t) $ go p
        Nothing -> \v -> case v of
          Yaml.String o -> go p o
          _ -> Aeson.typeMismatch "String" v
      ParseNumber mt p -> case mt of
        Just t -> Yaml.withScientific (T.unpack t) $ go p
        Nothing -> \v -> case v of
          Yaml.Number o -> go p o
          _ -> Aeson.typeMismatch "Number" v
      ParseArray mt p -> case mt of
        Just t -> Yaml.withArray (T.unpack t) $ go p
        Nothing -> \v -> case v of
          Yaml.Array o -> go p o
          _ -> Aeson.typeMismatch "Array" v
      ParseObject mt p -> case mt of
        Just t -> Yaml.withObject (T.unpack t) $ go p
        Nothing -> \v -> case v of
          Yaml.Object o -> go p o
          _ -> Aeson.typeMismatch "Object" v
      ParseList p -> \l -> forM l $ \v -> go p v
      ParseField key fp -> \o -> case fp of
        FieldParserRequired p -> do
          v <- o Yaml..: key
          go p v Aeson.<?> Aeson.Key key
        FieldParserOptional p -> do
          mv <- o Yaml..:? key
          case mv of
            Nothing -> pure Nothing
            Just v -> Just <$> go p v Aeson.<?> Aeson.Key key
        FieldParserOptionalWithDefault p d -> do
          mv <- o Yaml..:? key
          case mv of
            Nothing -> pure d
            Just v -> go p v Aeson.<?> Aeson.Key key
      ParsePure v -> const $ pure v
      ParseAp pf p -> \v -> go pf v <*> go p v
      ParseAlt ps -> \v -> case ps of
        [] -> fail "No alternatives."
        (p : ps') -> go p v <|> go (ParseAlt ps') v
      ParseFmap f p -> fmap f . go p
      ParseComment _ p -> go p

-- | Use a parser to produce a schema that describes it for documentation.
--
-- Nothing means that nothing even needs to be parsed, you just get the 'a' without parsing anything.
-- This is for the 'pure' case.
explainParser :: Parser i o -> Maybe Schema
explainParser = go
  where
    go :: Parser i o -> Maybe Schema
    go = \case
      ParseAny -> Just AnySchema
      ParseMaybe _ p -> go p
      ParseEq _ t _ -> Just $ ExactSchema t
      ParseBool t _ -> Just $ BoolSchema t
      ParseNumber t _ -> Just $ NumberSchema t
      ParseString t ParseAny -> Just $ StringSchema t
      ParseString _ p -> go p
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
      ParseAlt ps -> Just $ AltSchema $ catMaybes (map go ps)
      ParseComment t p -> CommentSchema t <$> go p

-- A schema for a parser.
--
-- This is used to produce documentation for what/how the parser parses.
data Schema
  = AnySchema
  | ExactSchema Text
  | BoolSchema (Maybe Text)
  | NumberSchema (Maybe Text)
  | StringSchema (Maybe Text)
  | ArraySchema (Maybe Text) Schema
  | ObjectSchema (Maybe Text) Schema
  | ListSchema Schema
  | FieldSchema Text Bool (Maybe Text) Schema
  | ApSchema Schema Schema -- We'll take this to mean 'and'
  | AltSchema [Schema]
  | CommentSchema Text Schema
  deriving (Show, Eq, Generic)

instance Validity Schema

-- | Render a schema as pretty text.
--
-- This is meant for humans.
-- The output may look like YAML but it is not.
prettySchema :: Schema -> Text
prettySchema = renderStrict . layoutPretty defaultLayoutOptions . schemaDoc

-- | A list of comments
newtype Comments = Comments {commentsList :: [Doc ()]}
  deriving (Show)

instance Semigroup Comments where
  (Comments l1) <> (Comments l2) = Comments $ l1 <> l2

instance Monoid Comments where
  mempty = emptyComments
  mappend = (<>)

-- | No comments
emptyComments :: Comments
emptyComments = Comments []

-- | A raw text as comments
comment :: Text -> Comments
comment t = Comments $ map pretty $ T.lines t

-- | Prettyprint a 'Schema'
schemaDoc :: Schema -> Doc ()
schemaDoc = go emptyComments
  where
    go :: Comments -> Schema -> Doc ()
    go cs =
      let g = go cs
          ge = go emptyComments
          mkComment :: Doc () -> Doc ()
          mkComment = ("# " <>)
          mkCommentsMDoc :: Comments -> Maybe (Doc ())
          mkCommentsMDoc = \case
            Comments [] -> Nothing
            Comments l -> Just $ align $ vsep $ map mkComment l
          addMComment :: Comments -> Maybe Text -> Comments
          addMComment c = \case
            Nothing -> c
            Just t -> c <> comment t
          e :: Doc () -> Comments -> Doc ()
          e s cs' =
            case mkCommentsMDoc cs' of
              Nothing -> s
              Just cd -> vsep [cd, s]
       in \case
            AnySchema -> e "<any>" cs
            ExactSchema t -> e (pretty t) cs
            BoolSchema t -> e "<bool>" $ addMComment cs t
            NumberSchema t -> e "<number>" $ addMComment cs t
            StringSchema t -> e "<string>" $ addMComment cs t
            ArraySchema t s -> "-" <+> align (go (addMComment cs t) s)
            -- The comments really only work on the object level
            -- so they are erased when going down
            ObjectSchema t s -> e (ge s) (addMComment cs t)
            ListSchema s -> g s
            FieldSchema k r md s ->
              let keyDoc :: Doc a
                  keyDoc = pretty k
                  requiredDoc :: Doc a
                  requiredDoc =
                    if r
                      then "required"
                      else case md of
                        Nothing -> "optional"
                        Just d -> "optional, default:" <+> pretty d
               in vsep
                    [ keyDoc <> ":" <+> mkComment requiredDoc,
                      indent 2 $ g s
                    ]
            ApSchema s1 s2 -> align $ vsep [g s1, g s2]
            AltSchema ss ->
              let listDoc :: [Doc a] -> Doc a
                  listDoc = \case
                    [] -> "[]"
                    (d : ds) -> vsep ["[" <+> nest 2 d, vsep $ map (("," <+>) . nest 2) ds, "]"]
               in e (listDoc $ map ge ss) cs
            CommentSchema t s -> go (cs <> comment t) s

-- | Helper function to implement 'FromJSON' via 'YamlSchema'
viaYamlSchema :: YamlSchema a => Yaml.Value -> Yaml.Parser a
viaYamlSchema = implementParser yamlSchema

-- | Helper function to add the schema documentation to the optparse applicative help output
confDesc :: Parser i o -> OptParse.InfoMod a
confDesc p = OptParse.footerDoc $ OptParse.string . T.unpack . prettySchema <$> explainParser p

-- | Helper function to read a config file for a type in 'YamlSchema'
readConfigFile :: (YamlSchema a, Yaml.FromJSON a) => Path r File -> IO (Maybe a)
readConfigFile p = readFirstConfigFile [p]

newtype ViaYamlSchema a = ViaYamlSchema a
  deriving (Show, Eq, Generic)

instance YamlSchema a => Yaml.FromJSON (ViaYamlSchema a) where
  parseJSON = fmap ViaYamlSchema . viaYamlSchema

-- | Helper function to read the first in a list of config files
readFirstConfigFile :: forall a r. (Yaml.FromJSON a, YamlSchema a) => [Path r File] -> IO (Maybe a)
readFirstConfigFile files = go files
  where
    go :: [Path r File] -> IO (Maybe a)
    go =
      \case
        [] -> pure Nothing
        (p : ps) -> do
          mc <- forgivingAbsence $ SB.readFile $ toFilePath p
          case mc of
            Nothing -> go ps
            Just contents ->
              case Yaml.decodeEither' contents of
                Left err -> do
                  let failedMsgs =
                        [ "Failed to parse yaml file",
                          toFilePath p,
                          "with error:",
                          Yaml.prettyPrintParseException err
                        ]
                      triedFilesMsgs = case files of
                        [] -> []
                        [f] -> ["While parsing file: " <> toFilePath f]
                        fs -> "While parsing files:" : (map (("* " <>) . toFilePath) fs)
                      referenceMsgs =
                        [ "Reference: ",
                          maybe "" (T.unpack . prettySchema) $ explainParser (yamlSchema :: YamlParser a)
                        ]
                  die
                    $ unlines
                    $ concat
                      [ failedMsgs,
                        triedFilesMsgs,
                        referenceMsgs
                      ]
                Right (ViaYamlSchema conf) -> pure $ Just conf
