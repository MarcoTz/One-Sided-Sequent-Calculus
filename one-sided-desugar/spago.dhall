{ name          = "one-sided-desugar"
, dependencies  = ["one-sided-defs","prelude","foldable-traversable"]
, packages      = ../packages.dhall
, sources       = [ "src/**/*.purs" ]
}
