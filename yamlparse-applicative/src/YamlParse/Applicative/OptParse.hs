{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module YamlParse.Applicative.OptParse where

import qualified Data.Text as T
import qualified Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse
import YamlParse.Applicative.Class
import YamlParse.Applicative.Explain
import YamlParse.Applicative.Parser
import YamlParse.Applicative.Pretty

-- | Helper function to add the schema documentation for a 'YamlSchema' parser to the optparse applicative help output
confDesc :: forall o a. YamlSchema o => OptParse.InfoMod a
confDesc = confDescWith (yamlSchema :: YamlParser o)

-- | Helper function to add the schema documentation for a given parser to the optparse applicative help output
confDescWith :: Parser i o -> OptParse.InfoMod a
confDescWith p = OptParse.footerDoc $ Just $ OptParse.string . T.unpack . prettyColourisedSchema $ explainParser p
