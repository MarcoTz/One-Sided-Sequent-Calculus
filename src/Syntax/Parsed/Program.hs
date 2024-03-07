module Syntax.Parsed.Program where 

import Common
import Syntax.Parsed.Terms
import Syntax.Parsed.Types

import Data.Map qualified as M

data XtorSig = MkXtorSig{sigName :: !XtorName, sigArgs :: ![Ty]} 

data DataDecl  = MkData  {declName  :: !TypeName, declArgs  :: ![PolVar], dataPol :: !Pol, declXtors :: ![XtorSig]} 
data VarDecl   = MkVar   {varName   :: !Variable, varBody   :: !Term}
data AnnotDecl = MkAnnot {annotName :: !Variable, annotType :: !Ty} 
newtype Import    = MkImport Modulename 

data Program = MkProgram { 
  progName    :: !Modulename, 
  progDecls   :: !(M.Map TypeName DataDecl), 
  progVars    :: !(M.Map Variable VarDecl), 
  progAnnots  :: !(M.Map Variable AnnotDecl),
  progImports :: ![Import]} 

emptyProg :: Modulename -> Program 
emptyProg mn = MkProgram mn M.empty M.empty M.empty []

addDeclProgram :: DataDecl -> Program -> Program 
addDeclProgram decl (MkProgram mn decls vars annots imports) = MkProgram mn (M.insert (declName decl) decl decls) vars annots imports

addVarProgram :: VarDecl -> Program -> Program 
addVarProgram var (MkProgram mn decls vars annots imports) = MkProgram mn decls (M.insert (varName var) var vars) annots imports

addAnnotProgram :: AnnotDecl -> Program -> Program 
addAnnotProgram annot (MkProgram mn decls vars annots imports) = MkProgram mn decls vars (M.insert (annotName annot) annot annots) imports

addImportProgram :: Import -> Program -> Program
addImportProgram imp (MkProgram mn decls vars annots imports) = MkProgram mn decls vars annots (imp:imports)

--data RecDecl  = MkRecDecl{recVar  :: !Variable, recTy :: !Ty, recBd :: !Term}
--data Eps = MkEps 
--newtype Codecl = MkCo DataDecl 
