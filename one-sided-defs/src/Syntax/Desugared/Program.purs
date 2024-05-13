module Syntax.Desugared.Program (
  XtorSig (..),
  VarDecl (..),
  DataDecl (..),
  Program (..),
  setMainProgram,
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
import Data.Maybe (Maybe(..),fromMaybe)
import Data.Map (Map,insert,empty,toUnfoldable)
import Data.Tuple (snd)

data XtorSig = XtorSig {sigPos :: Loc, sigName :: Xtorname, sigArgs :: List Ty} 
instance HasLoc XtorSig where 
  getLoc (XtorSig sig) = sig.sigPos
  setLoc loc (XtorSig sig) = XtorSig (sig {sigPos=loc})
instance Show XtorSig where 
  show (XtorSig sig) | null sig.sigArgs = show sig.sigName 
  show (XtorSig sig) = show sig.sigName <> "(" <> intercalate "," (show <$> sig.sigArgs)

data DataDecl = DataDecl {
  declPos   :: Loc, 
  declName  :: Typename, 
  declArgs  :: List VariantVar, 
  declType  :: DeclTy, 
  declXtors :: List XtorSig
} 
instance HasLoc DataDecl where 
  getLoc (DataDecl decl) = decl.declPos 
  setLoc loc (DataDecl decl) = DataDecl (decl {declPos=loc})
instance Show DataDecl where 
  show (DataDecl decl) | null decl.declArgs = 
    show decl.declType <> " " <> show decl.declName <> "{" <> intercalate ", " (show <$> decl.declXtors) <> "}"
  show (DataDecl decl) = 
    show decl.declType <> " " <> show decl.declName <> "(" <> intercalate ", " (show <$> decl.declArgs) <> ") { " <> intercalate ", " (show <$> decl.declXtors) <> "}"

data VarDecl  = VarDecl {varPos::Loc, varName::Variable, varIsRec::Boolean, varTy:: (Maybe Ty), varBody :: Term}
instance HasLoc VarDecl where 
  getLoc (VarDecl decl) = decl.varPos 
  setLoc loc (VarDecl decl) = VarDecl (decl {varPos=loc})
instance Show VarDecl where 
  show (VarDecl var) = do
    let recPref = if var.varIsRec then "rec " else ""
    let tyStr = fromMaybe "" ((\x -> " :: " <> show x) <$> var.varTy)
    recPref <> show var.varName <> tyStr <> " := " <> show var.varBody


data Program = Program { 
  progName  :: Modulename, 
  progDecls :: (Map Typename DataDecl), 
  progVars  :: (Map Variable VarDecl),
  progMain  :: (Maybe Command),
  progSrc   :: String
}  
instance Show Program where 
  show (Program prog) | Nothing == prog.progMain = 
    "\tModule " <> show prog.progName <> "\n" <>
    "\tDecalations:\n\t\t" <> showMap prog.progDecls <> "\n" <> 
    "\tVariables:\n\t\t" <> showMap  prog.progVars <> "\n" 
  show (Program prog) = 
    "\tModule " <> show prog.progName <> "\n" <>
    "\tDecalations:\n\t\t" <> showMap prog.progDecls <> "\n" <> 
    "\tVariables:\n\t\t" <> showMap  prog.progVars <> "\n" <> 
    "\tMain : " <> show prog.progMain

showMap :: forall a b.Show b => Map a b -> String 
showMap declMap = do
   let decls :: List b
       decls = snd <$> toUnfoldable declMap
   intercalate "\n\t\t" (show <$> decls)

emptyProg :: Modulename -> String -> Program 
emptyProg nm src = Program {progName:nm,progDecls:empty,progVars:empty,progMain:Nothing,progSrc:src}

addDeclProgram :: DataDecl -> Program -> Program 
addDeclProgram (DataDecl decl) (Program prog) = Program (prog { progDecls = insert decl.declName (DataDecl decl) prog.progDecls })

addVarProgram :: VarDecl -> Program -> Program
addVarProgram (VarDecl var) (Program prog) = Program (prog { progVars = insert var.varName (VarDecl var) prog.progVars})

setMainProgram :: Command -> Program -> Program 
setMainProgram c (Program prog) = Program (prog { progMain = Just c })

setSrcProgram :: String -> Program -> Program 
setSrcProgram src (Program prog) = Program (prog { progSrc = src })
