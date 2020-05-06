# Yamlparse applicative

DOESN'T ACTUALLY WORK YET.

Parse YAML config files like optparse-applicative.
Get both the documentation and the parsing from one value.

See also https://github.com/chrisdone/streaming-parsers for related work.



## Example

Schema implementation:

``` haskell
data MyConfig
  = MyConfig
      { myConfigText :: Text,
        myConfigScientific :: Maybe Scientific,
        myConfigList :: [Bool],
        myConfigSub :: Maybe MySubConfig
      }
  deriving (Show, Eq)

instance FromYamlSchema MyConfig where
  fromYamlSchema =
    object "MyConfig" $
      MyConfig
        <$> requiredField "foo"
        <*> optionalField "bar"
        <*> optionalFieldWithDefault "quux" []
```

Generated schema documentation:

```
# MyConfig
foo: # required
  <string>
bar: # optional
  <number>
quux: # optional, default: []
  - <bool>
```
