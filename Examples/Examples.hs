module Examples where 

import Typed.Types
import Typed.Program
import Untyped.Syntax
import Common

nilSig :: XtorSig 
nilSig = MkXtorSig{sigName = "Nil", sigArgs = []}
consSig :: XtorSig
consSig = MkXtorSig{sigName = "Cons", sigArgs = [TyVar "a" (MkKind Pos), TyDecl "List" [TyVar "a" (MkKind Pos)] (MkKind Pos)]}
listDecl :: Decl
listDecl = MkDataDecl{declNm = "List", declArgs = [("a",Pos)], declPol = Pos, declSig = [nilSig,consSig]}

tupSig :: XtorSig 
tupSig = MkXtorSig{sigName = "Tup", sigArgs=[TyVar "a" (MkKind Pos),TyVar "b" (MkKind Pos)]}
pairDecl :: Decl
pairDecl = MkDataDecl{declNm = "Pair", declArgs = [("a",Pos),("b",Pos)], declPol = Pos, declSig = [tupSig]}


headSig :: XtorSig 
headSig = MkXtorSig{sigName = "Head", sigArgs=[TyVar "a" (MkKind Neg)]}
tailSig :: XtorSig 
tailSig = MkXtorSig{sigName = "Tail", sigArgs=[TyDecl "Stream" [TyVar "a" (MkKind Neg)] (MkKind Neg)]}
streamDecl :: Decl 
streamDecl = MkDataDecl{declNm = "Stream", declArgs = [("a",Neg)], declPol = Neg, declSig=[headSig,tailSig]}

fstSig :: XtorSig 
fstSig = MkXtorSig{sigName = "Fst", sigArgs=[TyVar "a" (MkKind Neg)]}
sndSig :: XtorSig 
sndSig = MkXtorSig{sigName = "Snd", sigArgs=[TyVar "b" (MkKind Neg)]}
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
tys = [boolDecl, listDecl, pairDecl, streamDecl,lpairDecl]
terms :: [Term]
terms = [exList,exCase]
cmds :: [Command]
cmds = [exCmd]
