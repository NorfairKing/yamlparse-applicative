{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module YamlParse.Applicative.Pretty where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import YamlParse.Applicative.Class
import YamlParse.Applicative.Explain
import YamlParse.Applicative.Parser

-- | Render pretty documentation about the 'yamlSchema' of a type
--
-- This is meant for humans.
-- The output may look like YAML but it is not.
prettySchemaDoc :: forall a. YamlSchema a => Text
prettySchemaDoc = prettyParserDoc (yamlSchema @a)

-- | Render pretty documentation about a parser
--
-- This is meant for humans.
-- The output may look like YAML but it is not.
prettyParserDoc :: Parser i o -> Text
prettyParserDoc = prettySchema . explainParser

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
            EmptySchema -> e "# Nothing to parse" cs
            AnySchema -> e "<any>" cs
            ExactSchema t -> e (pretty t) cs
            NullSchema -> e "null" cs
            MaybeSchema s -> go (cs <> comment "or <null>") s
            BoolSchema t -> e "<bool>" $ addMComment cs t
            NumberSchema t -> e "<number>" $ addMComment cs t
            StringSchema t -> e "<string>" $ addMComment cs t
            ArraySchema t s -> "-" <+> align (go (addMComment cs t) s)
            -- The comments really only work on the object level
            -- so they are erased when going down
            ObjectSchema t s -> e (ge s) (addMComment cs t)
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
            ListSchema s -> g s
            MapSchema s -> e ("<key>: " <> nest 2 (g s)) cs
            MapKeysSchema s -> g s
            ApSchema s1 s2 -> align $ vsep [g s1, g s2]
            AltSchema ss ->
              let listDoc :: [Doc a] -> Doc a
                  listDoc = \case
                    [] -> "[]"
                    (d : ds) -> vsep ["[" <+> nest 2 d, vsep $ map (("," <+>) . nest 2) ds, "]"]
               in e (listDoc $ map ge ss) cs
            CommentSchema t s -> go (cs <> comment t) s
