{ name          = "one-sided-sc"
, dependencies  = [
  "one-sided-web-app", "one-sided-examples", "prelude","one-sided-defs",
  "console",
  "effect",
  "either",
  "foldable-traversable",
  "lists",
  "one-sided-driver",
  "ordered-collections",
  "strings",
  "tuples",
  "unfoldable",
 ]
, packages      = ./packages.dhall
, sources       = [ "src/**/*.purs", "test/**/*.purs" ]
}
