module Syntax.Parsed.Program where 

import Common
import Syntax.Parsed.Terms

import Data.Map qualified as M

data TypeScheme = MkTypeScheme ![TypeVar] !Ty

data Ty = TyVar !TypeVar | TyDecl !TypeName ![Ty]
data XtorSig = MkXtorSig{sigName :: !XtorName, sigArgs :: ![Ty]} 

data DataDecl  = MkData  {declName  :: !TypeName, dataArgs  :: ![(Variable,Pol)], dataPol :: !Pol, dataSig :: ![XtorSig]} 
data VarDecl   = MkVar   {varName   :: !Variable, varBody   :: !Term}
data AnnotDecl = MkAnnot {annotName :: !Variable, annotType :: !TypeScheme} 


data Program = MkProgram { progDecls :: !(M.Map TypeName DataDecl), progVars :: !(M.Map Variable VarDecl), progAnnots :: !(M.Map Variable AnnotDecl)} 

emptyProg :: Program 
emptyProg = MkProgram M.empty M.empty M.empty

addDeclProgram :: DataDecl -> Program -> Program 
addDeclProgram decl (MkProgram decls vars annots) = MkProgram (M.insert (declName decl) decl decls) vars annots 

addVarProgram :: VarDecl -> Program -> Program 
addVarProgram var (MkProgram decls vars annots) = MkProgram decls (M.insert (varName var) var vars) annots

addAnnotProgram :: AnnotDecl -> Program -> Program 
addAnnotProgram annot (MkProgram decls vars annots) = MkProgram decls vars (M.insert (annotName annot) annot annots)

--data RecDecl  = MkRecDecl{recVar  :: !Variable, recTy :: !Ty, recBd :: !Term}
--data Eps = MkEps 
--newtype Codecl = MkCo DataDecl 
