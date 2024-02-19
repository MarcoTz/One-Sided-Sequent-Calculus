module LPair where 

import Common
import Typed.Types 
import Typed.Program 

fstSig :: XtorSig 
fstSig = MkXtorSig{sigName = "Fst", sigArgs=[TyVar "a" (MkKind Neg)]}
sndSig :: XtorSig 
sndSig = MkXtorSig{sigName = "Snd", sigArgs=[TyVar "b" (MkKind Neg)]}
lpairDecl :: DataDecl
lpairDecl = MkDataDecl{declNm = "LPair", declArgs = [("a",Neg),("b",Neg)], declPol = Neg, declSig=[fstSig,sndSig]}
