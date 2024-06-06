let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.15-20240605/packages.dhall
        sha256:522a1ec6d2d3075f8584ab65cf308154be5d494fbd9c22ad32f4f11e64c1b652
in  upstream
with one-sided-checking   = ./one-sided-checking/spago.dhall  as Location
with one-sided-defs       = ./one-sided-defs/spago.dhall      as Location
with one-sided-depcheck   = ./one-sided-depcheck/spago.dhall  as Location
with one-sided-desugar    = ./one-sided-desugar/spago.dhall   as Location
with one-sided-driver     = ./one-sided-driver/spago.dhall    as Location
with one-sided-eval       = ./one-sided-eval/spago.dhall      as Location
with one-sided-examples   = ./one-sided-examples/spago.dhall  as Location 
with one-sided-inference  = ./one-sided-inference/spago.dhall as Location
with one-sided-kinding    = ./one-sided-kinding/spago.dhall   as Location
with one-sided-parser     = ./one-sided-parser/spago.dhall    as Location 
with one-sided-web-app    = ./one-sided-web/spago.dhall       as Location
