module Pretty where 

import Syntax 
import TypeInference.Types
import Data.List (intercalate) 

instance Show Pol where 
  show Pos = "+"
  show Neg = "-"

instance Show Command where 
  show (Cut t1 pol t2) = "<" <> show t1 <> " | " <> show pol <> " | " <> show t2 <> ">"

instance Show Pattern where 
  show MkPattern{ptxt=xt, ptv=vars, ptcmd=cmd} = xt <> "(" <> intercalate ", " (show <$> vars) <> ") => " <> show cmd

instance Show Term where 
  show (Var v) = v
  show (Mu v cmd) = "mu " <> v <> ". " <> show cmd
  show (Xtor xt args) = xt <> "(" <> intercalate ", " (show <$> args)
  show (XCase pts) = "case {" <>  intercalate ", " (show <$> pts) <> "}"
  show (Shift t) = "{" <> show t <> "}"
  show (Lam v cmd) = "Lambda {" <> v <> "}." <> show cmd

instance Show Ty where 
  show (TyVar v) = v 
  show (TyDecl nm args) = nm <> "(" <> intercalate ", " (show <$> args) <> ")"
  show (TyShift ty) = "{" <> show ty <> "}"
  show (TyCo ty) = "co " <> show ty

instance Show Decl where 
  show MkDataDecl{declNm=nm, declArgs=args, declPol=pl, declSig=sig} = 
    "data " <> nm <> "(" <> intercalate ", " ((\(v,p) -> v <> ":" <> show p) <$> args) <> ") :" <> show pl <> " where { " <> show sig <> "}"
  show MkValDecl{valVar = v, valTy=ty, valBd=bd} = 
    "val " <> v <> ":" <> show ty <> " = " <> show bd
  show MkRecDecl{recVar = v, recTy=ty, recBd=bd} = "rec " <> v <> ": " <> show ty <> " = " <> show bd
  show MkEps = "epsilon"
  show (MkCoDecl d) = "co " <> show d


instance Show XtorSig where 
  show MkXtorSig{sigName = nm, sigArgs=args} = nm <> "(" <> intercalate ", " (show <$> args) <> ")"
