module Syntax.Typed.Program (
  Program (..),
  VarDecl (..),
  DataDecl (..),
  XtorSig (..),
  addVarProgram,
  addDeclProgram,
  setMainProgram,
  setSrcProgram,
  emptyProg,
  progIsEmpty,
  embedXtorSig
) where 

import Common (Xtorname, Variable, Typename, VariantVar, DeclTy, Modulename) 
import Loc (Loc,class HasLoc)
import Syntax.Typed.Types (Ty,embedType)
import Syntax.Typed.Terms (Term, Command)
import Syntax.Desugared.Program (XtorSig(..)) as D

import Prelude ((&&), class Show, show, (<>), (<$>))
import Data.Map (Map,empty, insert,isEmpty)
import Data.List (List,null,intercalate)
import Data.Maybe (Maybe (..),isNothing)

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

data VarDecl = VarDecl {varPos::Loc, varName::Variable, varIsRec::Boolean, varTy::Ty,varBody::Term}
instance HasLoc VarDecl where 
  getLoc (VarDecl decl) = decl.varPos 
  setLoc loc (VarDecl decl) = VarDecl (decl {varPos=loc})
instance Show VarDecl where 
  show (VarDecl decl) = do 
   let recStr = if decl.varIsRec then "rec " else ""
   recStr <> show decl.varName <> ": " <> show decl.varTy <> " := " <> show decl.varBody

data Program = Program {
  progName  :: Modulename, 
  progDecls :: (Map Typename DataDecl), 
  progVars  :: (Map Variable VarDecl),
  progMain  :: (Maybe Command),
  progSrc   :: String
}
instance Show Program where 
  show (Program prog) = 
    "Module " <> show prog.progName <> "\n" <> 
    "Declarations: " <> show prog.progDecls <> "\n" <> 
    "Variables: " <> show prog.progVars <> "\n" <> 
    "Main: " <> show prog.progMain

emptyProg :: Modulename -> Program
emptyProg nm = Program {progName:nm,progDecls:empty,progVars:empty,progMain:Nothing,progSrc:""}

progIsEmpty :: Program -> Boolean
progIsEmpty (Program {progName:_,progDecls:decls,progVars:vars,progMain:main}) = 
  isEmpty decls && isEmpty vars && isNothing main

addDeclProgram :: DataDecl -> Program -> Program
addDeclProgram (DataDecl decl) (Program prog) = Program (prog { progDecls = insert decl.declName (DataDecl decl) prog.progDecls })

addVarProgram :: VarDecl -> Program -> Program
addVarProgram (VarDecl var) (Program prog) = Program (prog { progVars = insert var.varName (VarDecl var) prog.progVars })

setMainProgram :: Command -> Program -> Program
setMainProgram c (Program prog) = Program (prog { progMain = Just c })

setSrcProgram :: String -> Program -> Program
setSrcProgram src  (Program prog) = Program (prog{ progSrc = src })
