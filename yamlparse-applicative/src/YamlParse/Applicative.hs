{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Yamlparse applicative
--
-- Implement Yaml parsing and get documentation for it for free.
--
--
-- = Usage example
--
-- For more examples, see the `yamlparse-applicative-demo` examples [in the github repository](https://github.com/NorfairKing/yamlparse-applicative/blob/master/yamlparse-applicative-demo/src/YamlParse/Applicative/Demo.hs)
--
-- Suppose you have some tool and you want to have it read its configuration from a file.
-- You make a type for the configuration:
--
-- > data Configuration
-- >   = Configuration
-- >   { confUrl :: Maybe Text
-- >   , confPort :: Int
-- >   , confToken :: Text
-- >   } deriving (Show, Eq)
--
-- Instead of implementing a 'Data.Yaml.FromJSON' instance, you now implement a 'YamlSchema' instance like so:
--
-- > instance YamlSchema Configuration where
-- >   yamlSchema =
-- >     objectParser $ -- Declare that it is a Yaml object
-- >       Configuration
-- >         <$> optionalField -- An optional key may be in the file
-- >             "url"
-- >             "The url to host the server at. It will be hosted on 'localhost' by default."
-- >         <*> optionalFieldWithDefault -- An optional key with default _may_ in the file, the default will be used otherwise
-- >             "port"
-- >             8000
-- >             "The post to host the server at."
-- >         <*> requiredField -- A required field must be in the file
-- >             "token"
-- >             "The authorisation token that clients can use to authenticate themselves."
--
-- Now you've already documented the configuration in code.
-- This will make sure that your documentation stays correct because it will be type-checked.
--
-- Now you can implement 'Data.Yaml.FromJSON' like so:
--
-- > instance FromJSON Configuration where
-- >   parseJSON = viaYamlSchema
--
-- And you can get user-facing documentation about the format for free using 'prettySchema . explainParser':
--
-- > # Configuration
-- > url: # optional
-- >   # The url to host the server at. It will be hosted on 'localhost' by default.
-- >   <string>
-- > port: # optional, default: 8000
-- >   # The post to host the server at.
-- >   <number>
-- > token: # required
-- >   # The authorisation token that clients can use to authenticate themselves.
-- >   <bool>
--
-- If you are also using 'optparse-applicative', you can even add this documentation to your `--help` page as well using 'confDesc':
--
-- > argParser :: ParserInfo SomethingElse
-- > argParser =
-- >   info
-- >     (helper <$> parseSomethingElse)
-- >     (confDesc @Configuration)
module YamlParse.Applicative
  ( -- * The YamlSchema Class
    YamlSchema (..),
    YamlKeySchema (..),

    -- ** Implementing YamlSchema instances
    objectParser,
    unnamedObjectParser,
    maybeParser,
    eitherParser,
    extraParser,
    (<?>),
    (<??>),
    requiredField,
    requiredField',
    requiredFieldWith,
    requiredFieldWith',
    optionalField,
    optionalField',
    optionalFieldWith,
    optionalFieldWith',
    optionalFieldWithDefault,
    optionalFieldWithDefault',
    optionalFieldWithDefaultWith,
    optionalFieldWithDefaultWith',
    viaRead,
    literalString,
    literalValue,
    literalShowValue,
    alternatives,

    -- * Parser
    YamlParser,
    ObjectParser,
    Parser (..),

    -- * Using a yaml schema

    -- ** Parsing Yaml via a YamlSchema instance
    viaYamlSchema,
    ViaYamlSchema (..),

    -- ** Getting a parser implemntation from a 'Parser'
    implementParser,

    -- * Documentation for a 'Parser'
    prettySchemaDoc,
    prettyParserDoc,
    prettyColourisedSchemaDoc,
    prettyColourisedParserDoc,

    -- ** Parser schemas
    explainParser,
    Schema (..),

    -- ** Showing the schema to the user
    prettySchema,
    prettyColourisedSchema,

    -- * Interface with 'optparse-applicative'
    confDesc,

    -- * Parsing a file
    readConfigFile,
    readFirstConfigFile,
  )
where

import YamlParse.Applicative.Class
import YamlParse.Applicative.Explain
import YamlParse.Applicative.IO
import YamlParse.Applicative.Implement
import YamlParse.Applicative.OptParse
import YamlParse.Applicative.Parser
import YamlParse.Applicative.Pretty
