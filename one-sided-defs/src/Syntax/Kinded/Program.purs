module Syntax.Kinded.Program (
  Program (..),
  RecDecl (..),
  getRecTy,
  VarDecl (..),
  getVarTy,
  DataDecl (..),
  getXtors,
  XtorSig (..),
  getXtorname,
  addRecProgram,
  getRecsProgram,
  addVarProgram,
  getVarsProgram,
  addDeclProgram,
  getDeclsProgram,
  setMainProgram,
  getMainProgram,
  setSrcProgram,
  getSrcProgram,
  emptyProg,
  isEmpty
) where 


import Loc (Loc, class HasLoc) 
import Common (Xtorname,Typename,Variable, VariantVar, DeclTy, Modulename)
import Syntax.Kinded.Types (Ty)
import Syntax.Kinded.Terms (Term, Command)

import Prelude ((&&), (==), class Show, show, (<>), (<$>))
import Data.List (List, null, intercalate)
import Data.Map (Map,insert, empty)
import Data.Map (isEmpty) as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

data XtorSig = XtorSig{sigPos :: Loc, sigName :: Xtorname, sigArgs :: List Ty} 
instance HasLoc XtorSig where 
  getLoc (XtorSig sig) = sig.sigPos
  setLoc loc (XtorSig sig) = XtorSig (sig {sigPos=loc}) 
instance Show XtorSig where
  show (XtorSig sig) | null sig.sigArgs = show sig.sigName
  show (XtorSig sig) = show sig.sigName <> "(" <> intercalate ", " (show <$> sig.sigArgs)
getXtorname :: XtorSig -> Xtorname
getXtorname (XtorSig sig) = sig.sigName

data DataDecl = DataDecl {declPos :: Loc, declName :: Typename, declArgs :: List VariantVar, declType :: DeclTy, declXtors :: List XtorSig} 
instance HasLoc DataDecl where 
  getLoc (DataDecl decl) = decl.declPos
  setLoc loc (DataDecl decl) = DataDecl (decl {declPos=loc})
instance Show DataDecl where 
  show (DataDecl decl) | null decl.declArgs = 
    show decl.declType <>  " " <> show decl.declName <> "{" <> intercalate ", " (show <$> decl.declXtors)
  show (DataDecl decl) = 
    show decl.declType <> " " <> show decl.declName <> "(" <> intercalate ", " (show <$> decl.declArgs) <> ") {" <> intercalate ", " (show <$> decl.declXtors)
getXtors :: DataDecl -> List XtorSig 
getXtors (DataDecl decl) = decl.declXtors

data VarDecl = VarDecl {varPos::Loc, varName::Variable, varTy::Ty, varBody::Term}
instance HasLoc VarDecl where 
  getLoc (VarDecl decl) = decl.varPos
  setLoc loc (VarDecl decl) = VarDecl (decl {varPos=loc})
instance Show VarDecl where 
  show (VarDecl decl) = show decl.varName <> " : " <> show decl.varTy <> " := " <> show decl.varBody
getVarTy :: VarDecl -> Tuple Variable Ty 
getVarTy (VarDecl decl) = Tuple decl.varName decl.varTy

data RecDecl   = RecDecl  {recPos  :: Loc, recName  :: Variable, recTy    :: Ty,       recBody :: Term}
instance HasLoc RecDecl where 
  getLoc (RecDecl decl) = decl.recPos
  setLoc loc (RecDecl decl) = RecDecl (decl {recPos=loc}) 
instance Show RecDecl where 
  show (RecDecl decl) = "rec " <> show decl.recName <> ": " <> show decl.recTy <> " := " <> show decl.recBody
getRecTy :: RecDecl -> Tuple Variable Ty
getRecTy (RecDecl decl) = Tuple decl.recName decl.recTy

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
    "Module " <> show prog.progName <> "\n " <>
    "Declarations: " <> show prog.progDecls <> "\n " <>
    "Variables: " <> show prog.progVars <> "\n "<>
    "Recursive Definitions: " <> show prog.progRecs <> "\n" 
  show (Program prog) = 
    "Module " <> show prog.progName <> "\n " <>
    "Declarations: " <> show prog.progDecls <> "\n " <>
    "Variables: " <> show prog.progVars <> "\n "<>
    "Recursive Definitions: " <> show prog.progRecs <> "\n"  <>
    "Main: " <> show prog.progMain


emptyProg :: Modulename -> String -> Program
emptyProg nm src = Program {progName:nm, progDecls:empty, progVars:empty, progRecs:empty, progMain:Nothing, progSrc:src}

isEmpty :: Program -> Boolean
isEmpty (Program prog) =  M.isEmpty prog.progDecls && M.isEmpty prog.progVars && M.isEmpty prog.progRecs && Nothing == prog.progMain

addDeclProgram :: DataDecl -> Program -> Program
addDeclProgram (DataDecl decl) (Program prog) = Program (prog { progDecls = insert decl.declName (DataDecl decl) prog.progDecls })
getDeclsProgram :: Program -> Map Typename DataDecl
getDeclsProgram (Program prog) = prog.progDecls

addVarProgram :: VarDecl -> Program -> Program
addVarProgram (VarDecl var) (Program prog) = Program (prog { progVars = insert var.varName (VarDecl var) prog.progVars })
getVarsProgram :: Program -> Map Variable VarDecl
getVarsProgram (Program prog) = prog.progVars

addRecProgram :: RecDecl -> Program -> Program
addRecProgram (RecDecl rec) (Program prog) = Program (prog { progRecs = insert rec.recName (RecDecl rec) prog.progRecs })
getRecsProgram :: Program -> Map Variable RecDecl
getRecsProgram (Program prog) = prog.progRecs

setMainProgram :: Command -> Program -> Program
setMainProgram c (Program prog) = Program (prog { progMain = Just c })
getMainProgram :: Program -> Maybe Command 
getMainProgram (Program prog) = prog.progMain

setSrcProgram :: String -> Program -> Program
setSrcProgram src (Program prog) = Program (prog { progSrc = src })
getSrcProgram :: Program -> String 
getSrcProgram (Program prog) = prog.progSrc
