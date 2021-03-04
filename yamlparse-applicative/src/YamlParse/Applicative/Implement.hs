{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module YamlParse.Applicative.Implement where

import Control.Applicative
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import YamlParse.Applicative.Parser

-- | Use a 'Parser' to parse a value from Yaml.
--
-- A 'Parser i o' corresponds exactly to a 'i -> Yaml.Parser o' and this function servers as evidence for that.
implementParser :: Parser i o -> (i -> Yaml.Parser o)
implementParser = go
  where
    go :: Parser i o -> (i -> Yaml.Parser o)
    go = \case
      ParseAny -> pure
      ParseExtra ef p -> \i -> do
        o <- go p i
        ef o
      ParseEq v t p -> \i -> do
        r <- go p i
        if r == v then pure r else fail $ "Expected " <> T.unpack t <> " exactly but got: " <> show r
      ParseNull -> \v -> case v of
        Yaml.Null -> pure ()
        _ -> fail $ "Expected 'null' but got: " <> show v
      ParseMaybe p -> \v -> case v of
        Yaml.Null -> pure Nothing
        _ -> Just <$> go p v
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
      ParseField key fp -> \o -> case fp of
        FieldParserRequired p -> do
          v <- o Yaml..: key
          go p v
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
      ParseList p -> mapM (go p)
      ParseMap p -> HM.traverseWithKey $ \_ v -> go p v
      ParseMapKeys p pm -> \val -> do
        hm <- go pm val
        M.fromList <$> mapM (\(k, v) -> (,) <$> go p k <*> pure v) (HM.toList hm)
      ParsePure v -> const $ pure v
      ParseAp pf p -> \v -> go pf v <*> go p v
      ParseAlt ps -> \v -> case ps of
        [] -> fail "No alternatives."
        [p] -> go p v
        (p' : ps') -> go p' v <|> go (ParseAlt ps') v
      ParseFmap f p -> fmap f . go p
      ParseComment _ p -> go p
