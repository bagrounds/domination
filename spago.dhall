{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "halogen"
  , "psci-support"
  , "generics-rep"
  , "profunctor-lenses"
  , "foldable-traversable"
  , "transformers"
  , "aff"
  , "web-dom"
  , "argonaut"
  , "argonaut-generic"
  , "random"
  , "monad-loops"
  , "web-storage"
  , "unordered-collections"
  , "arraybuffer-class"
  , "arraybuffer-types"
  , "webaudio"

  -- test dependencies
  , "quickcheck"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
