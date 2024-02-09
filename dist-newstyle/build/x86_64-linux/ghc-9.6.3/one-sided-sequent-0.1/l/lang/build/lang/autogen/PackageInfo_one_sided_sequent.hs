{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_one_sided_sequent (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "one_sided_sequent"
version :: Version
version = Version [0,1] []

synopsis :: String
synopsis = "one sided sequent calculus language"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
