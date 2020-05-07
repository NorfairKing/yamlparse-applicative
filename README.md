# Yamlparse applicative

DOESN'T ACTUALLY WORK YET.

Parse YAML config files like optparse-applicative.
Get both the documentation and the parsing from one value.

See also https://github.com/chrisdone/streaming-parsers for related work.



## Example

(See more under `yamlparse-applicative-demo`)

Schema implementation:

``` haskell
data MyConfig
  = MyConfig
      { myConfigText :: Text,
        myConfigScientific :: Maybe Scientific,
        myConfigList :: [Bool],
      }
  deriving (Show, Eq)

instance YamlSchema MyConfig where
  yamlSchema =
    object "MyConfig" $
      MyConfig
        <$> requiredField "foo" "My foo docs"
        <*> optionalField "bar" "My bar docs"
        <*> optionalFieldWithDefault "quux" [] "My quux docs"
```

Generated schema documentation:

```
# MyConfig
foo: # required
  # My foo docs
  <string>
bar: # optional
  # My bar docs
  <number>
quux: # optional, default: []
  - # My quux docs
    <bool>
```
