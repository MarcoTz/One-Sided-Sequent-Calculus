module Syntax.Parsed.Program where 

import Common
import Syntax.Parsed.Terms
import Syntax.Parsed.Types

import Data.Map qualified as M

data XtorSig = MkXtorSig{sigName :: !XtorName, sigArgs :: ![Ty]} 

data DataDecl  = MkData  {declName  :: !TypeName, declArgs  :: ![PolVar], dataPol :: !Pol, declXtors :: ![XtorSig]} 
data VarDecl   = MkVar   {varName   :: !Variable, varBody   :: !Term}
data AnnotDecl = MkAnnot {annotName :: !Variable, annotType :: !Ty} 

data Program = MkProgram { progName :: !Modulename, progDecls :: !(M.Map TypeName DataDecl), progVars :: !(M.Map Variable VarDecl), progAnnots :: !(M.Map Variable AnnotDecl)} 

emptyProg :: Modulename -> Program 
emptyProg mn = MkProgram mn M.empty M.empty M.empty

addDeclProgram :: DataDecl -> Program -> Program 
addDeclProgram decl (MkProgram mn decls vars annots) = MkProgram mn (M.insert (declName decl) decl decls) vars annots 

addVarProgram :: VarDecl -> Program -> Program 
addVarProgram var (MkProgram mn decls vars annots) = MkProgram mn decls (M.insert (varName var) var vars) annots

addAnnotProgram :: AnnotDecl -> Program -> Program 
addAnnotProgram annot (MkProgram mn decls vars annots) = MkProgram mn decls vars (M.insert (annotName annot) annot annots)

--data RecDecl  = MkRecDecl{recVar  :: !Variable, recTy :: !Ty, recBd :: !Term}
--data Eps = MkEps 
--newtype Codecl = MkCo DataDecl 
