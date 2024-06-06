{ name          = "one-sided-depcheck"
, dependencies  = ["one-sided-defs","transformers","prelude"]
, packages      = ../packages.dhall
, sources       = [ "src/**/*.purs" ]
}
