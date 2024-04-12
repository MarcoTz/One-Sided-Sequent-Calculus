module Pretty.Parsed () where 

import Pretty.Common ()
import Syntax.Parsed.Terms
import Syntax.Parsed.Types
import Syntax.Parsed.Program

import Data.List (intercalate)
import Data.Map qualified as M

instance Show Term where 
  show (Var _ v) = show v
  show (Mu _ v cmd) = "mu " <> show v <> ". " <> show cmd
  show (Xtor _ xt []) = show xt
  show (Xtor _ xt args) = show xt <> "(" <> intercalate ", " (show <$> args) <> ")"
  show (XCase _ pts) = "case {" <>  intercalate ", " (show <$> pts) <> "}"
  show (ShiftCBV _ t) = "{" <> show t <> ":CBV}"
  show (ShiftCBN _ t) = "{" <> show t <> ":CBN}" 

instance Show Pattern where 
  show (MkPattern xt [] cmd) = show xt <> " => " <> show cmd
  show (MkPattern xt vars cmd) = show xt <> "(" <> intercalate ", " (show <$> vars) <> ") => " <> show cmd

instance Show Command where 
  show (Cut _ t pol u) = "〈" <> show t <> " | " <> show pol <> " | " <> show u <> "〉"
  show (CutAnnot _ t ty pol u) = "〈" <> show t <> " | " <> show ty <> " | " <> show pol <> " | " <> show u <> "〉" 
  show (Done _) = "Done"
  show (Err _ err) = "error " <> err
  show (Print _ t) = "Print " <> show t
  show (PrintAnnot _ t ty) = " Print " <> show t <> " :: " <> show ty

instance Show KindedTy where 
  show (KindedTy ty pol) = show ty<> " : " <> show pol

instance Show Ty where 
  show (TyVar v) = show v 
  show (TyDecl nm []) = show nm
  show (TyDecl nm args) = show nm <> "(" <> intercalate ", " (show <$> args) <> ")"
  show (TyCo ty) = "co " <> show ty
  show (TyShift ty) = "{" <> show ty <> "}"
  show (TyForall args ty) = "forall " <> intercalate ", " (show <$> args) <> ". " <> show ty


instance Show DataDecl where 
  show (MkData _ n [] pol sigs) = "data " <> show n <> ": " <> show pol <> "{" <> intercalate ", " (show <$> sigs) <> "}"
  show (MkData _ n args pol sigs) =  
    "data " <> show n <> "(" <> intercalate ", " (show <$> args) <> ") : " <> show pol <> " {" <> intercalate ",  " (show <$> sigs) <> "}"

instance Show XtorSig where 
  show (MkXtorSig _ nm [])   = show nm
  show (MkXtorSig _ nm args) = show nm <> "(" <> intercalate ", " (show <$> args) <> ")"

instance Show VarDecl where
  show (MkVar _ n t) = show n <> ":=" <> show t <> ";"

instance Show RecDecl where 
  show (MkRec _ n t) = "rec "<>show n <> ":=" <> show t <> ";"

instance Show AnnotDecl where 
  show (MkAnnot _ n ty) = show n <> " :: " <> show ty

instance Show Import where 
  show (MkImport _ mn) = "import " <> show mn

instance Show Program where 
  show (MkProgram nm decls vars recs annots imports Nothing src) = 
    "module " <> show nm  <>
    "\n\tImports: " <> intercalate "," (show <$> imports) <> 
    "\n\tDeclarations: " <> show (snd <$> M.toList decls) <> 
    "\n\tVariables: " <> show (snd <$> M.toList vars) <> 
    "\n\tRecursive Variables: " <> show (snd <$> M.toList recs) <>
    "\n\tAnnotations: " <> show (snd <$> M.toList annots) <> 
    "\n\tSource: " <> src
  show (MkProgram nm decls vars recs annots imports (Just c) src) = 
    show (MkProgram nm decls vars recs annots imports Nothing src) <> 
    "\n\t Main: "<>show c
