{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module YamlParse.Applicative.Pretty where

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Text.Prettyprint.Doc.Render.Util.StackMachine
import YamlParse.Applicative.Class
import YamlParse.Applicative.Explain
import YamlParse.Applicative.Parser

data Colour = Yellow | Gray | Red | Blue | White

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

-- | Render pretty colourised documentation about the 'yamlSchema' of a type
--
-- This is meant for humans.
-- The output may look like YAML but it is not.
prettyColourisedSchemaDoc :: forall a. YamlSchema a => Text
prettyColourisedSchemaDoc = prettyColourisedParserDoc (yamlSchema @a)

-- | Render pretty colourised documentation about a parser
--
-- This is meant for humans.
-- The output may look like YAML but it is not.
prettyColourisedParserDoc :: Parser i o -> Text
prettyColourisedParserDoc = prettyColourisedSchema . explainParser

-- | Render a schema as pretty text.
--
-- This is meant for humans.
-- The output may look like YAML but it is not.
prettySchema :: Schema -> Text
prettySchema = renderStrict . layoutPretty defaultLayoutOptions . schemaDoc

-- | Render a schema as pretty and colourised text.
--
-- This is meant for humans.
-- The output may look like YAML but it is not.
prettyColourisedSchema :: Schema -> Text
prettyColourisedSchema = renderSimplyDecorated id startColour resetColour . layoutPretty defaultLayoutOptions . schemaDoc
  where
    startColour :: Colour -> Text
    startColour = \case
      Yellow -> "\x1b[33m"
      Gray -> "\x1b[2m"
      Red -> "\x1b[31m"
      Blue -> "\x1b[34m"
      White -> "\x1b[37m"
    resetColour :: Colour -> Text
    resetColour _ = "\x1b[0m"

-- | A list of comments
newtype Comments = Comments {commentsList :: [Doc Colour]}
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
schemaDoc :: Schema -> Doc Colour
schemaDoc = go emptyComments
  where
    go :: Comments -> Schema -> Doc Colour
    go cs =
      let g = go cs
          ge = go emptyComments
          mkComment :: Doc Colour -> Doc Colour
          mkComment = ("# " <>)
          mkCommentsMDoc :: Comments -> Maybe (Doc Colour)
          mkCommentsMDoc = \case
            Comments [] -> Nothing
            Comments l -> Just $ align $ vsep $ map (annotate Gray . mkComment) l
          addMComment :: Comments -> Maybe Text -> Comments
          addMComment c = \case
            Nothing -> c
            Just t -> c <> comment t
          e :: Doc Colour -> Comments -> Doc Colour
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
            ObjectSchema t AnySchema -> e "<object>" (addMComment cs t)
            ObjectSchema t s -> e (ge s) (addMComment cs t)
            FieldSchema k r md s ->
              let keyDoc :: Doc Colour
                  keyDoc = pretty k
                  requiredDoc :: Doc Colour
                  requiredDoc =
                    if r
                      then annotate Red "required"
                      else case md of
                        Nothing -> blueOptional
                        Just d -> blueOptional <+> ", default:" <+> pretty d
                    where
                      blueOptional = annotate Blue "optional"
               in vsep
                    [ annotate White keyDoc <> ":" <+> mkComment requiredDoc,
                      indent 2 $ g s
                    ]
            ListSchema s -> g s
            MapSchema s -> e (annotate White "<key>: " <> nest 2 (g s)) cs
            MapKeysSchema s -> g s
            ApSchema s1 s2 -> align $ vsep [g s1, g s2]
            AltSchema ss ->
              let listDoc :: [Doc Colour] -> Doc Colour
                  listDoc = \case
                    [] -> "[]"
                    (d : ds) ->
                      vsep
                        [ "[" <+> nest 2 d,
                          vsep $ map (("," <+>) . nest 2) ds,
                          "]"
                        ]
               in listDoc $ map ge ss
            CommentSchema t s -> go (cs <> comment t) s
