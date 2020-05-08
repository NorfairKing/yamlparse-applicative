{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module YamlParse.Applicative.Explain where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)
import YamlParse.Applicative.Parser

-- A schema for a parser.
--
-- This is used to produce documentation for what/how the parser parses.
data Schema
  = EmptySchema
  | AnySchema
  | ExactSchema Text
  | NullSchema
  | MaybeSchema Schema
  | BoolSchema (Maybe Text)
  | NumberSchema (Maybe Text)
  | StringSchema (Maybe Text)
  | ArraySchema (Maybe Text) Schema
  | ObjectSchema (Maybe Text) Schema
  | FieldSchema Text Bool (Maybe Text) Schema
  | ListSchema Schema
  | MapSchema Schema
  | MapKeysSchema Schema
  | ApSchema Schema Schema -- We'll take this to mean 'and'
  | AltSchema [Schema]
  | CommentSchema Text Schema
  deriving (Show, Eq, Generic)

instance Validity Schema

-- | Use a parser to produce a schema that describes it for documentation.
--
-- Nothing means that nothing even needs to be parsed, you just get the 'a' without parsing anything.
-- This is for the 'pure' case.
explainParser :: Parser i o -> Schema
explainParser = go
  where
    go :: Parser i o -> Schema
    go = \case
      ParseAny -> AnySchema
      ParseExtra _ p -> go p
      ParseEq _ t _ -> ExactSchema t
      ParseNull -> NullSchema
      ParseMaybe p -> MaybeSchema $ go p
      ParseBool t _ -> BoolSchema t
      ParseNumber t _ -> NumberSchema t
      ParseString t ParseAny -> StringSchema t
      ParseString _ p -> go p
      ParseArray t p -> ArraySchema t $ go p
      ParseObject t p -> ObjectSchema t $ go p
      ParseField k fp -> case fp of
        FieldParserRequired p -> FieldSchema k True Nothing $ go p
        FieldParserOptional p -> FieldSchema k False Nothing $ go p
        FieldParserOptionalWithDefault p d -> FieldSchema k False (Just $ T.pack $ show d) $ go p
      ParseList p -> ListSchema $ go p
      ParseMap p -> MapSchema $ go p
      ParseMapKeys _ p -> MapKeysSchema $ go p
      ParsePure _ -> EmptySchema
      ParseFmap _ p -> go p
      ParseAp pf p -> ApSchema (go pf) (go p)
      ParseAlt ps -> AltSchema $ map go ps
      ParseComment t p -> CommentSchema t $ go p
