module Syntax.Parsed.Program where 

import Common
import Syntax.Parsed.Terms
import Syntax.Parsed.Types

import Data.Map qualified as M

data XtorSig = MkXtorSig{sigName :: !XtorName, sigArgs :: ![Ty]} 

data DataDecl  = MkData   {declName   :: !TypeName, declArgs  :: ![PolVar], dataPol   :: !Pol, declXtors :: ![XtorSig]} 
data VarDecl   = MkVar    {varName    :: !Variable, varBody   :: !Term}
data AnnotDecl = MkAnnot  {annotName  :: !Variable, annotType :: !PolTy} 
newtype Import = MkImport {importName :: Modulename }

data Program = MkProgram { 
  progName    :: !Modulename, 
  progDecls   :: !(M.Map TypeName DataDecl), 
  progVars    :: !(M.Map Variable VarDecl), 
  progAnnots  :: !(M.Map Variable AnnotDecl),
  progImports :: ![Import],
  progMain    :: !(Maybe Command)} 

emptyProg :: Modulename -> Program 
emptyProg mn = MkProgram mn M.empty M.empty M.empty [] Nothing

addDeclProgram :: DataDecl -> Program -> Program 
addDeclProgram decl (MkProgram mn decls vars annots imports main) = MkProgram mn (M.insert (declName decl) decl decls) vars annots imports  main

addVarProgram :: VarDecl -> Program -> Program 
addVarProgram var (MkProgram mn decls vars annots imports main) = MkProgram mn decls (M.insert (varName var) var vars) annots imports main

addAnnotProgram :: AnnotDecl -> Program -> Program 
addAnnotProgram annot (MkProgram mn decls vars annots imports main) = MkProgram mn decls vars (M.insert (annotName annot) annot annots) imports main

addImportProgram :: Import -> Program -> Program
addImportProgram imp (MkProgram mn decls vars annots imports main) = MkProgram mn decls vars annots (imp:imports) main

setMainProgram :: Command -> Program -> Program
setMainProgram c (MkProgram mn decls vars annots imports _) = MkProgram mn decls vars annots imports (Just c)

--data RecDecl  = MkRecDecl{recVar  :: !Variable, recTy :: !Ty, recBd :: !Term}
--data Eps = MkEps 
--newtype Codecl = MkCo DataDecl 
