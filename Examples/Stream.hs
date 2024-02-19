module Stream where 

import Common
import Typed.Types
import Typed.Program


headSig :: XtorSig 
headSig = MkXtorSig{sigName = "Head", sigArgs=[TyVar "a" (MkKind Neg)]}
tailSig :: XtorSig 
tailSig = MkXtorSig{sigName = "Tail", sigArgs=[TyDecl "Stream" [TyVar "a" (MkKind Neg)] (MkKind Neg)]}
streamDecl :: Decl 
streamDecl = MkDataDecl{declNm = "Stream", declArgs = [("a",Neg)], declPol = Neg, declSig=[headSig,tailSig]}
