{ name          = "one-sided-parser"
, dependencies  = ["one-sided-defs","prelude","debug","parsing","strings"]
, packages      = ../packages.dhall
, sources       = [ "src/**/*.purs" ]
}
