{ name          = "one-sided-web-app"
, dependencies  = ["one-sided-defs","one-sided-eval","one-sided-driver","debug","halogen","web-dom","web-html","prelude","effect","console","transformers"]
, packages      = ../packages.dhall
, sources       = [ "src/**/*.purs" ]
}
