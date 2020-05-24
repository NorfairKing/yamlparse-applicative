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
import Data.Maybe
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import YamlParse.Applicative.Class
import YamlParse.Applicative.Explain
import YamlParse.Applicative.Parser
import Data.Text.Prettyprint.Doc.Render.Util.StackMachine

data Color = Yellow | Gray | Red | Blue

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

-- | Render a schema as pretty and colorized text.
--
-- This is meant for humans.
-- The output may look like YAML but it is not.
prettyColorizedSchema :: Schema -> Text
prettyColorizedSchema = renderSimplyDecorated id startColor resetColor . layoutPretty defaultLayoutOptions . schemaDoc
  where startColor :: Color -> Text
        startColor = \case
          Yellow -> "\x1b[33m"
          Gray -> "\x1b[2m"
          Red -> "\x1b[31m"
          Blue -> "\x1b[34m"
        resetColor :: Color -> Text
        resetColor _ = "\x1b[0m"

-- | A list of comments
newtype Comments = Comments {commentsList :: [Doc Color]}
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
schemaDoc :: Schema -> Doc Color
schemaDoc = go emptyComments
  where
    go :: Comments -> Schema -> Doc Color
    go cs =
      let g = go cs
          ge = go emptyComments
          mkComment :: Doc Color -> Doc Color
          mkComment = ("# " <>)
          mkCommentsMDoc :: Comments -> Maybe (Doc Color)
          mkCommentsMDoc = \case
            Comments [] -> Nothing
            Comments l -> Just $ align $ vsep $ map ((annotate Gray) . mkComment) l
          addMComment :: Comments -> Maybe Text -> Comments
          addMComment c = \case
            Nothing -> c
            Just t -> c <> comment t
          e :: Doc Color -> Comments -> Doc Color
          e s cs' =
            case mkCommentsMDoc cs' of
              Nothing -> annotate Yellow s
              Just cd -> vsep [cd, annotate Yellow s]
       in \case
            EmptySchema -> e emptyDoc $ addMComment cs $ Just "Nothing to parse"
            AnySchema -> e "<any>" cs
            ExactSchema t -> e (pretty t) cs <+> fromJust (mkCommentsMDoc $ comment "(exact)")
            NullSchema -> e "null" cs
            MaybeSchema s -> go (cs <> comment "or <null>") s
            BoolSchema t -> e "<boolean>" $ addMComment cs t
            NumberSchema t -> e "<number>" $ addMComment cs t
            StringSchema t -> e "<string>" $ addMComment cs t
            ArraySchema t s -> "-" <+> align (go (addMComment cs t) s)
            -- The comments really only work on the object level
            -- so they are erased when going down
            ObjectSchema t s -> vsep
              [ e "<object>" (addMComment cs t)
              , ge s
              ]
            FieldSchema k r md s ->
              let keyDoc :: Doc Color
                  keyDoc = pretty k
                  requiredDoc :: Doc Color
                  requiredDoc =
                    if r
                      then annotate Red "required"
                      else case md of
                        Nothing -> blueOptional
                        Just d -> blueOptional <+> ", default:" <+> pretty d
                    where blueOptional = annotate Blue "optional"
               in vsep
                    [ keyDoc <> ":" <+> mkComment requiredDoc,
                      indent 2 $ g s
                    ]
            ListSchema s -> g s
            MapSchema s -> e ("<key>: " <> nest 2 (g s)) cs
            MapKeysSchema s -> g s
            ApSchema s1 s2 -> align $ vsep [g s1, g s2]
            AltSchema ss ->
              let listDoc :: [Doc Color] -> Doc Color
                  listDoc = \case
                    [] -> "[]"
                    (d : ds) -> vsep
                      [ "[" <+> nest 2 d
                      , vsep $ map (("," <+>) . nest 2) ds
                      , "]"
                      ]
              in listDoc $ map ge ss
            CommentSchema t s -> go (cs <> comment t) s
