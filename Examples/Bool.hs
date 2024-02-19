module Bool where 

import Typed.Program
import Common 

trueSig :: XtorSig 
trueSig = MkXtorSig{sigName="True", sigArgs=[]}
falseSig :: XtorSig
falseSig = MkXtorSig{sigName="False", sigArgs=[]}
boolDecl :: Decl
boolDecl = MkDataDecl{declNm="Bool", declArgs=[], declPol=Pos, declSig=[trueSig,falseSig]}
