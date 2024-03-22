module Pretty.Parsed () where 

import Pretty.Common ()
import Syntax.Parsed.Terms
import Syntax.Parsed.Types
import Syntax.Parsed.Program

import Data.List (intercalate)
import Data.Map qualified as M

instance Show Term where 
  show (Var v) = show v
  show (Mu v cmd) = "mu " <> show v <> ". " <> show cmd
  show (Xtor xt []) = show xt
  show (Xtor xt args) = show xt <> "(" <> intercalate ", " (show <$> args) <> ")"
  show (XCase pts) = "case {" <>  intercalate ", " (show <$> pts) <> "}"
  show (ShiftPos t) = "{" <> show t <> "}"
  show (ShiftNeg v cmd) = "{" <> show v <> "}." <> show cmd

instance Show Pattern where 
  show (MkPattern xt [] cmd) = show xt <> " => " <> show cmd
  show (MkPattern xt vars cmd) = show xt <> "(" <> intercalate ", " (show <$> vars) <> ") => " <> show cmd

instance Show Command where 
  show (Cut t pol u) = "<" <> show t <> " | " <> show pol <> " | " <> show u <> ">"
  show (CutAnnot t ty pol u) = "<" <> show t <> " | " <> show ty <> " | " <> show pol <> " | " <> show u <> ">"
  show Done = "Done"
  show (Err err) = "error " <> err

instance Show PolTy where 
  show (MkPolTy ty pol) = show ty<> " : " <> show pol

instance Show Ty where 
  show (TyVar v) = show v 
  show (TyDecl nm []) = show nm
  show (TyDecl nm args) = show nm <> "(" <> intercalate ", " (show <$> args) <> ")"
  show (TyCo ty) = "co " <> show ty
  show (TyShift ty) = "{" <> show ty <> "}"
  show (TyForall args ty) = "forall " <> intercalate ", " (show <$> args) <> ". " <> show ty


instance Show DataDecl where 
  show (MkData n [] pol sigs) = "data " <> show n <> ": " <> show pol <> "{" <> intercalate ", " (show <$> sigs) <> "}"
  show (MkData n args pol sigs) =  
    "data " <> show n <> "(" <> intercalate ", " (show <$> args) <> ") : " <> show pol <> " {" <> intercalate ",  " (show <$> sigs) <> "}"

instance Show XtorSig where 
  show (MkXtorSig nm [])   = show nm
  show (MkXtorSig nm args) = show nm <> "(" <> intercalate ", " (show <$> args) <> ")"

instance Show VarDecl where
  show (MkVar n t) = show n <> ":=" <> show t <> ";"

instance Show RecDecl where 
  show (MkRec n t) = "rec "<>show n <> ":=" <> show t <> ";"

instance Show AnnotDecl where 
  show (MkAnnot n ty) = show n <> " :: " <> show ty

instance Show Import where 
  show (MkImport mn) = "import " <> show mn

instance Show Program where 
  show (MkProgram nm decls vars recs annots imports Nothing) = 
    "module " <> show nm  <>
    "\n\tImports: " <> intercalate "," (show <$> imports) <> 
    "\n\tDeclarations: " <> show (snd <$> M.toList decls) <> 
    "\n\tVariables: " <> show (snd <$> M.toList vars) <> 
    "\n\tRecursive Variables: " <> show (snd <$> M.toList recs) <>
    "\n\tAnnotations: " <> show (snd <$> M.toList annots)
  show (MkProgram nm decls vars recs annots imports (Just c)) = 
    show (MkProgram nm decls vars recs annots imports Nothing) <> 
    "\n\t Main: "<>show c
