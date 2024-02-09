module Examples where 

import TwoSided.Program
import TwoSided.Types
import TwoSided.Syntax

nilSig :: XtorSig Ctor 
nilSig = MkXtorSig {sigName = "Nil", sigXtor = CtorRep, sigProdArgs = [], sigConsArgs = []}
consSig :: XtorSig Ctor 
consSig = MkXtorSig {sigName = "Cons", sigXtor=CtorRep, sigProdArgs = [TyVar "a", TyDeclared "List" [TyVar "a"]], sigConsArgs = []}
listDecl :: Declaration
listDecl = MkDeclaration { declName = "List", declTypeArgs = [("a",Pos)], declPol = Pos, declSig = [nilSig,consSig] }

tupSig :: XtorSig Ctor
tupSig = MkXtorSig {sigName = "Tup", sigXtor=CtorRep, sigProdArgs = [TyVar "a", TyVar "b"], sigConsArgs = []}
pairDecl :: Declaration
pairDecl = MkDeclaration { declName = "Pair", declTypeArgs = [("a",Pos),("b",Pos)], declPol = Pos, declSig = [tupSig]}

headSig :: XtorSig Dtor 
headSig = MkXtorSig {sigName ="Head", sigXtor=DtorRep, sigProdArgs = [], sigConsArgs = [TyVar "a"]}
tailSig :: XtorSig Dtor
tailSig = MkXtorSig {sigName="Tail", sigXtor=DtorRep, sigProdArgs = [], sigConsArgs = [TyDeclared "Stream" [TyVar "a"]]}
streamDecl :: Declaration
streamDecl = MkCodeclaration{ codeclName = "Stream", codeclTypeArgs = [("a",Pos)], codeclPol = Neg, codeclInterface = [headSig,tailSig]}

