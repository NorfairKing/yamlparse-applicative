# Yamlparse applicative

Parse YAML config files like optparse-applicative.
Get both the documentation and the parsing from one value.

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
    objectParser "MyConfig" $
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

## Related work

Chris done has made something like this with a focus on security instead of a focus on documentation: https://github.com/chrisdone/streaming-parsers
