module Examples where 

import TwoSided.Program
import TwoSided.Types
import TwoSided.Syntax

nilSig :: XtorSig Ctor 
nilSig = MkXtorSig {sigName = "Nil", sigXtor = CtorRep, sigProdArgs = [], sigConsArgs = []}
consSig :: XtorSig Ctor 
consSig = MkXtorSig {sigName = "Cons", sigXtor=CtorRep, sigProdArgs = [TyVar "a", TyDeclared "List" [TyVar "a"]], sigConsArgs = []}
listDecl :: Declaration
listDecl = DataDecl (MkTyDecl { declName = "List", declTyArgs = [("a",Pos)], declPol = Pos, declSig = [nilSig,consSig] })

tupSig :: XtorSig Ctor
tupSig = MkXtorSig {sigName = "Tup", sigXtor=CtorRep, sigProdArgs = [TyVar "a", TyVar "b"], sigConsArgs = []}
pairDecl :: Declaration
pairDecl = DataDecl (MkTyDecl { declName = "Pair", declTyArgs = [("a",Pos),("b",Pos)], declPol = Pos, declSig = [tupSig]})

headSig :: XtorSig Dtor 
headSig = MkXtorSig {sigName ="Head", sigXtor=DtorRep, sigProdArgs = [], sigConsArgs = [TyVar "a"]}
tailSig :: XtorSig Dtor
tailSig = MkXtorSig {sigName="Tail", sigXtor=DtorRep, sigProdArgs = [], sigConsArgs = [TyDeclared "Stream" [TyVar "a"]]}
streamDecl :: Declaration
streamDecl = CodataDecl (MkTyDecl { declName = "Stream", declTyArgs = [("a",Pos)], declPol = Neg, declSig = [headSig,tailSig]})

fstSig :: XtorSig Dtor
fstSig = MkXtorSig {sigName = "Fst", sigXtor=DtorRep, sigProdArgs = [], sigConsArgs = [TyVar "a"]}
sndSig :: XtorSig Dtor 
sndSig = MkXtorSig {sigName = "Snd", sigXtor=DtorRep, sigProdArgs = [], sigConsArgs = [TyVar "b"]}
lPairDecl :: Declaration
lPairDecl = CodataDecl (MkTyDecl { declName = "LPair", declTyArgs = [("a",Pos),("b",Pos)], declPol=Neg, declSig =[fstSig,sndSig]})

tys :: [Declaration]
tys = [listDecl, pairDecl, streamDecl,lPairDecl]

