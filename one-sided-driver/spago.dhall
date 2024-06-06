{ name          = "one-sided-driver"
, dependencies  = ["one-sided-defs","one-sided-eval","one-sided-parser","one-sided-depcheck","one-sided-desugar","one-sided-inference","one-sided-checking","one-sided-kinding","prelude","transformers"]
, packages      = ../packages.dhall
, sources       = [ "src/**/*.purs" ]
}
