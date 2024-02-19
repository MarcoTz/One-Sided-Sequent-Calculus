module List where 

import Typed.Program 
import Typed.Types
import Untyped.Syntax
import Common 

nilSig :: XtorSig 
nilSig = MkXtorSig{sigName = "Nil", sigArgs = []}
consSig :: XtorSig
consSig = MkXtorSig{sigName = "Cons", sigArgs = [TyVar "a" (MkKind Pos), TyDecl "List" [TyVar "a" (MkKind Pos)] (MkKind Pos)]}
listDecl :: DataDecl
listDecl = MkDataDecl{declNm = "List", declArgs = [("a",Pos)], declPol = Pos, declSig = [nilSig,consSig]}


exList :: Term
exList = Xtor "Cons" [Xtor "True" [], Xtor "Cons" [Xtor "False" [] , Xtor "Nil" []]]
exCase :: Term 
exCase = XCase [MkPattern{ptxt="Nil",ptv=[],ptcmd=Cut (Xtor "False" []) Pos (Var "x")}, MkPattern{ptxt="Cons",ptv=["a","b"], ptcmd=Cut (Var "a") Pos (Var "x") }]

exCmd :: Command 
exCmd = Cut exList Pos exCase
