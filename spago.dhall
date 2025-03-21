{ name = "cardano-transaction-builder"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "bytearrays"
  , "cardano-types"
  , "datetime"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "maybe"
  , "mote"
  , "mote-testplan"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "record"
  , "spec"
  , "transformers"
  , "tuples"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
