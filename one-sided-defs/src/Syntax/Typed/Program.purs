module Syntax.Typed.Program (
  Program (..),
  RecDecl (..),
  VarDecl (..),
  DataDecl (..),
  XtorSig (..),
  addRecProgram,
  addVarProgram,
  addDeclProgram,
  setMainProgram,
  setSrcProgram,
  emptyProg,
  isEmpty,
  embedXtorSig
) where 

import Common (Xtorname, Variable, Typename, VariantVar, DeclTy, Modulename) 
import Loc (Loc,class HasLoc)
import Syntax.Typed.Types (Ty,embedType)
import Syntax.Typed.Terms (Term, Command)
import Syntax.Desugared.Program (XtorSig(..)) as D

import Prelude ((&&), (==), class Show, show, (<>), (<$>))
import Data.Map (Map,empty, insert)
import Data.Map (isEmpty) as M
import Data.List (List,null,intercalate)
import Data.Maybe (Maybe (..))

data XtorSig = XtorSig {sigPos::Loc, sigName::Xtorname, sigArgs::List Ty} 
instance HasLoc XtorSig where 
  getLoc (XtorSig sig) = sig.sigPos
  setLoc loc (XtorSig sig) = XtorSig (sig {sigPos=loc})  
instance Show XtorSig where
  show (XtorSig sig) | null sig.sigArgs = show sig.sigName
  show (XtorSig sig) = show sig.sigName <> "(" <> intercalate ", " (show <$> sig.sigArgs) <> ")"

embedXtorSig :: XtorSig -> D.XtorSig 
embedXtorSig (XtorSig sig) = D.XtorSig {sigPos:sig.sigPos, sigName:sig.sigName, sigArgs:embedType <$> sig.sigArgs}

data DataDecl = DataDecl {declPos :: Loc, declName :: Typename, declArgs :: List VariantVar, declType :: DeclTy, declXtors :: List XtorSig} 
instance HasLoc DataDecl where 
  getLoc (DataDecl decl) = decl.declPos
  setLoc loc (DataDecl decl) = DataDecl (decl {declPos=loc}) 
instance Show DataDecl where 
  show (DataDecl decl) | null decl.declArgs = show decl.declName <> "{" <> intercalate ", " (show <$> decl.declXtors) <> "}"
  show (DataDecl decl) = show decl.declName <> "(" <> intercalate ", " (show <$> decl.declArgs) <> ") {" <> intercalate ", " (show <$> decl.declXtors) <> "}"

data VarDecl = VarDecl {varPos::Loc, varName::Variable, varTy::Ty,varBody::Term}
instance HasLoc VarDecl where 
  getLoc (VarDecl decl) = decl.varPos 
  setLoc loc (VarDecl decl) = VarDecl (decl {varPos=loc})
instance Show VarDecl where 
  show (VarDecl decl) = show decl.varName <> ": " <> show decl.varTy <> " := " <> show decl.varBody

data RecDecl = RecDecl  {recPos::Loc, recName::Variable, recTy::Ty,recBody::Term}
instance HasLoc RecDecl where 
  getLoc (RecDecl decl) = decl.recPos 
  setLoc loc (RecDecl decl) = RecDecl (decl {recPos=loc}) 
instance Show RecDecl where 
  show (RecDecl decl) = "rec " <> show decl.recName <> ": " <> show decl.recTy <> " := " <> show decl.recBody

data Program = Program {
  progName  :: Modulename, 
  progDecls :: (Map Typename DataDecl), 
  progVars  :: (Map Variable VarDecl),
  progRecs  :: (Map Variable RecDecl),
  progMain  :: (Maybe Command),
  progSrc   :: String
}
instance Show Program where 
  show (Program prog) = 
    "Module " <> show prog.progName <> "\n" <> 
    "Declarations: " <> show prog.progDecls <> "\n" <> 
    "Variables: " <> show prog.progVars <> "\n" <> 
    "Recursive Definitions: " <> show prog.progRecs <> "\n" <> 
    "Main: " <> show prog.progMain

emptyProg :: Modulename -> Program
emptyProg nm = Program {progName:nm,progDecls:empty,progVars:empty,progRecs:empty,progMain:Nothing,progSrc:""}

isEmpty :: Program -> Boolean
isEmpty (Program prog) = M.isEmpty prog.progDecls && M.isEmpty prog.progVars && M.isEmpty prog.progRecs && Nothing == prog.progMain

addDeclProgram :: DataDecl -> Program -> Program
addDeclProgram (DataDecl decl) (Program prog) = Program (prog { progDecls = insert decl.declName (DataDecl decl) prog.progDecls })

addVarProgram :: VarDecl -> Program -> Program
addVarProgram (VarDecl var) (Program prog) = Program (prog { progVars = insert var.varName (VarDecl var) prog.progVars })

addRecProgram :: RecDecl -> Program -> Program
addRecProgram (RecDecl rec) (Program prog) = Program (prog { progRecs = insert rec.recName (RecDecl rec) prog.progRecs })

setMainProgram :: Command -> Program -> Program
setMainProgram c (Program prog) = Program (prog { progMain = Just c })

setSrcProgram :: String -> Program -> Program
setSrcProgram src  (Program prog) = Program (prog{ progSrc = src })
