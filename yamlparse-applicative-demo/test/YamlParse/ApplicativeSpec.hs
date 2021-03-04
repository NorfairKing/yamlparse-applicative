{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module YamlParse.ApplicativeSpec where

import qualified Data.Aeson.Types as Aeson
import Data.GenValidity.Aeson ()
import Data.GenValidity.Containers ()
import Data.GenValidity.UnorderedContainers ()
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Scientific (Scientific)
import Data.Set (Set)
import Data.Text (Text)
import Data.Typeable
import GHC.Generics (Generic)
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Utils
import YamlParse.Applicative as Yaml

spec :: Spec
spec =
  describe "implementParser" $ do
    implementationsSpec @Bool
    implementationsSpec @Char
    implementationsSpec @String
    implementationsSpec @Text
    implementationsSpec @Scientific
    implementationsSpec @Aeson.Array
    implementationsSpec @Aeson.Object
    implementationsSpec @Aeson.Value
    implementationsSpec @[Text]
    implementationsSpec @(NonEmpty Text)
    implementationsSpec @(Maybe Text)
    implementationsSpec @(Set Text)
    implementationsSpec @(Map Text Int)
    implementationsSpec @(HashMap Text Int)
    implementationsSpec @(Map String Int)
    implementationsSpec @Fruit

data Fruit = Apple | Banana | Melon
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON Fruit

instance Aeson.ToJSON Fruit

instance YamlSchema Fruit where
  yamlSchema =
    alternatives
      [ literalShowValue Apple,
        literalShowValue Banana,
        literalShowValue Melon
      ]

instance Validity Fruit

instance GenUnchecked Fruit

instance GenValid Fruit

implementationsSpec ::
  forall a.
  ( Show a,
    Eq a,
    Typeable a,
    GenValid a,
    Aeson.FromJSON a,
    Aeson.ToJSON a,
    YamlSchema a
  ) =>
  Spec
implementationsSpec = specify ("The implementation of 'parseJSON' matches the implementation of 'implementParser yamlSchema' for " <> nameOf @a) $ implementationsMatch @a

implementationsMatch ::
  forall a.
  ( Show a,
    Eq a,
    GenValid a,
    Aeson.FromJSON a,
    Aeson.ToJSON a,
    YamlSchema a
  ) =>
  Property
implementationsMatch =
  forAllValid $
    \a -> do
      let v = Aeson.toJSON (a :: a)
      let aesonResult = Aeson.parseEither Aeson.parseJSON v :: Either String a
          yamlResult = Aeson.parseEither (implementParser yamlSchema) v
      yamlResult `shouldBe` aesonResult
