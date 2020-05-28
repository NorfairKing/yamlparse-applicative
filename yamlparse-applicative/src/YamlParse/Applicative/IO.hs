{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module YamlParse.Applicative.IO where

import qualified Data.ByteString as SB
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Path
import Path.IO
import System.Exit
import YamlParse.Applicative.Class
import YamlParse.Applicative.Explain
import YamlParse.Applicative.Parser
import YamlParse.Applicative.Pretty

-- | Helper function to read a config file for a type in 'YamlSchema'
readConfigFile :: (YamlSchema a, Yaml.FromJSON a) => Path r File -> IO (Maybe a)
readConfigFile p = readFirstConfigFile [p]

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
                        fs -> "While parsing files:" : map (("* " <>) . toFilePath) fs
                      referenceMsgs =
                        [ "Reference: ",
                          T.unpack $ prettyColourisedSchema $ explainParser (yamlSchema :: YamlParser a)
                        ]
                  die
                    $ unlines
                    $ concat
                      [ failedMsgs,
                        triedFilesMsgs,
                        referenceMsgs
                      ]
                Right (ViaYamlSchema conf) -> pure $ Just conf
