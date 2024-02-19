module Examples where 

import Typed.Types
import Untyped.Syntax

nilSig :: XtorSig 
nilSig = MkXtorSig{sigName = "Nil", sigArgs = []}
consSig :: XtorSig
consSig = MkXtorSig{sigName = "Cons", sigArgs = [TyVar "a", TyDecl "List" [TyVar "a"]]}
listDecl :: Decl
listDecl = MkDataDecl{declNm = "List", declArgs = [("a",Pos)], declPol = Pos, declSig = [nilSig,consSig]}

tupSig :: XtorSig 
tupSig = MkXtorSig{sigName = "Tup", sigArgs=[TyVar "a",TyVar "b"]}
pairDecl :: Decl
pairDecl = MkDataDecl{declNm = "Pair", declArgs = [("a",Pos),("b",Pos)], declPol = Pos, declSig = [tupSig]}


headSig :: XtorSig 
headSig = MkXtorSig{sigName = "Head", sigArgs=[TyVar "a"]}
tailSig :: XtorSig 
tailSig = MkXtorSig{sigName = "Tail", sigArgs=[TyDecl "Stream" [TyVar "a"]]}
streamDecl :: Decl 
streamDecl = MkDataDecl{declNm = "Stream", declArgs = [("a",Neg)], declPol = Neg, declSig=[headSig,tailSig]}

fstSig :: XtorSig 
fstSig = MkXtorSig{sigName = "Fst", sigArgs=[TyVar "a"]}
sndSig :: XtorSig 
sndSig = MkXtorSig{sigName = "Snd", sigArgs=[TyVar "b"]}
lpairDecl :: Decl
lpairDecl = MkDataDecl{declNm = "LPair", declArgs = [("a",Neg),("b",Neg)], declPol = Neg, declSig=[fstSig,sndSig]}

trueSig :: XtorSig 
trueSig = MkXtorSig{sigName="True", sigArgs=[]}
falseSig :: XtorSig
falseSig = MkXtorSig{sigName="False", sigArgs=[]}
boolDecl :: Decl
boolDecl = MkDataDecl{declNm="Bool", declArgs=[], declPol=Pos, declSig=[trueSig,falseSig]}

exList :: Term
exList = Xtor "Cons" [Xtor "True" [], Xtor "Cons" [Xtor "False" [] , Xtor "Nil" []]]
exCase :: Term 
exCase = XCase [MkPattern{ptxt="Nil",ptv=[],ptcmd=Cut (Xtor "False" []) Pos (Var "x")}, MkPattern{ptxt="Cons",ptv=["a","b"], ptcmd=Cut (Var "a") Pos (Var "x") }]

exCmd :: Command 
exCmd = Cut exList Pos exCase


tys :: [Decl]
tys = [listDecl, pairDecl, streamDecl,lpairDecl]
terms :: [Term]
terms = [exList,exCase]
cmds :: [Command]
cmds = [exCmd]
