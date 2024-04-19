module Syntax.Desugared.Program (
  XtorSig (..),
  VarDecl (..),
  RecDecl (..),
  DataDecl (..),
  Program (..),
  setMainProgram,
  addRecProgram,
  addVarProgram,
  addDeclProgram,
  setSrcProgram,
  emptyProg
) where 

import Loc (Loc, class HasLoc)
import Common (Xtorname,Variable,Typename,VariantVar,DeclTy, Modulename)
import Syntax.Desugared.Types (Ty)
import Syntax.Desugared.Terms (Term,Command)

import Prelude (class Show,show,(<>), (<$>), (==))
import Data.List (List, null, intercalate)
import Data.Maybe (Maybe(..))
import Data.Map (Map,insert,empty)

XtorSig = {sigPos :: Loc, sigName :: Xtorname, sigArgs :: List Ty} 
instance HasLoc XtorSig where 
  getLoc sig = sig.sigPos
  setLoc loc sig = sig {sigPos=loc}
instance Show XtorSig where 
  show sig | null sig.sigArgs = show sig.sigName 
  show sig = show sig.sigName <> "(" <> intercalate "," (show <$> sig.sigArgs)

data DataDecl = DataDecl {declPos :: Loc, declName :: Typename, declArgs :: List VariantVar, declType :: DeclTy, declXtors :: List XtorSig} 
instance HasLoc DataDecl where 
  getLoc (DataDecl decl) = decl.declPos 
  setLoc loc (DataDecl decl) = DataDecl (decl {declPos=loc})
instance Show DataDecl where 
  show (DataDecl decl) | null decl.declArgs = 
    show decl.declType <> " " <> show decl.declName <> "{" <> intercalate ", " (show <$> decl.declXtors) <> "}"
  show (DataDecl decl) = 
    show decl.declType <> " " <> show decl.declName <> "(" <> intercalate ", " (show <$> decl.declArgs) <> ") { " <> intercalate ", " (show <$> decl.declXtors) <> "}"

data VarDecl  = VarDecl {varPos::Loc, varName::Variable, varTy:: (Maybe Ty), varBody :: Term}
instance HasLoc VarDecl where 
  getLoc (VarDecl decl) = decl.varPos 
  setLoc loc (VarDecl decl) = VarDecl (decl {varPos=loc})
instance Show VarDecl where 
  show (VarDecl decl) | Nothing == decl.varTy = 
     show decl.varName <> ":= " <> show decl.varBody
  show (VarDecl decl) = show decl.varName <> ":" <> show decl.varTy <> ":=" <> show decl.varBody

data RecDecl  = RecDecl {recPos::Loc, recName::Variable, recTy::(Maybe Ty), recBody::Term}
instance HasLoc RecDecl where 
  getLoc (RecDecl decl) = decl.recPos
  setLoc loc (RecDecl decl) = RecDecl (decl {recPos=loc}) 
instance Show RecDecl where 
  show (RecDecl decl) | Nothing == decl.recTy = 
    "rec " <> show decl.recName <> ":= " <> show decl.recBody
  show (RecDecl decl) = " rec " <> show decl.recName <> ":" <> show decl.recTy <> " := " <> show decl.recBody

data Program = Program { 
  progName  :: Modulename, 
  progDecls :: (Map Typename DataDecl), 
  progVars  :: (Map Variable VarDecl),
  progRecs  :: (Map Variable RecDecl),
  progMain  :: (Maybe Command),
  progSrc   :: String
}  
instance Show Program where 
  show (Program prog) | Nothing == prog.progMain = 
    "Module " <> show prog.progName <> "\n" <>
    "Decalations: " <> show prog.progDecls <> "\n" <> 
    "Variables: " <> show  prog.progVars <> "\n" <> 
    "Recursive Definitions" <> show prog.progRecs <> "\n"
  show (Program prog) = 
    "Module " <> show prog.progName <> "\n" <>
    "Decalations: " <> show prog.progDecls <> "\n" <> 
    "Variables: " <> show  prog.progVars <> "\n" <> 
    "Recursive Definitions" <> show prog.progRecs <> "\n" <> 
    "Main : " <> show prog.progMain

emptyProg :: Modulename -> String -> Program 
emptyProg nm src = Program {progName:nm,progDecls:empty,progVars:empty,progRecs:empty,progMain:Nothing,progSrc:src}

addDeclProgram :: DataDecl -> Program -> Program 
addDeclProgram (DataDecl decl) (Program prog) = Program (prog { progDecls = insert decl.declName (DataDecl decl) prog.progDecls })

addVarProgram :: VarDecl -> Program -> Program
addVarProgram (VarDecl var) (Program prog) = Program (prog { progVars = insert var.varName (VarDecl var) prog.progVars})

addRecProgram :: RecDecl -> Program -> Program
addRecProgram (RecDecl rec) (Program prog) = Program (prog { progRecs = insert rec.recName (RecDecl rec) prog.progRecs })

setMainProgram :: Command -> Program -> Program 
setMainProgram c (Program prog) = Program (prog { progMain = Just c })

setSrcProgram :: String -> Program -> Program 
setSrcProgram src (Program prog) = Program (prog { progSrc = src })
