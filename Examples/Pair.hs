module Pair where 

import Common
import Typed.Program
import Typed.Types

tupSig :: XtorSig 
tupSig = MkXtorSig{sigName = "Tup", sigArgs=[TyVar "a" (MkKind Pos),TyVar "b" (MkKind Pos)]}
pairDecl :: Decl
pairDecl = MkDataDecl{declNm = "Pair", declArgs = [("a",Pos),("b",Pos)], declPol = Pos, declSig = [tupSig]}
