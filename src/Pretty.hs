module Pretty where 

import Untyped.Syntax qualified as S
import Typed.Syntax   qualified as T
import Typed.Types    qualified as T
import Typed.Program  qualified as T
import EmbedTyped
import Common

import Data.List (intercalate) 

instance Show Pol where 
  show Pos = "+"
  show Neg = "-"

instance Show S.Command where 
  show (S.Cut t1 pol t2) = "<" <> show t1 <> " | " <> show pol <> " | " <> show t2 <> ">"
instance Show T.Command where 
  show c = show $ (embed :: T.Command -> S.Command) c

instance Show S.Pattern where 
  show S.MkPattern{S.ptxt=xt, S.ptv=vars, S.ptcmd=cmd} = xt <> "(" <> intercalate ", " (show <$> vars) <> ") => " <> show cmd

instance Show S.Term where 
  show (S.Var v) = v
  show (S.Mu v cmd) = "mu " <> v <> ". " <> show cmd
  show (S.Xtor xt args) = xt <> "(" <> intercalate ", " (show <$> args)
  show (S.XCase pts) = "case {" <>  intercalate ", " (show <$> pts) <> "}"
  show (S.Shift t) = "{" <> show t <> "}"
  show (S.Lam v cmd) = "Lambda {" <> v <> "}." <> show cmd
instance Show T.Term where 
  show t = show $ (embed :: T.Term -> S.Term) t 

instance Show T.Ty where 
  show (T.TyVar v _) = v 
  show (T.TyDecl nm args _) = nm <> "(" <> intercalate ", " (show <$> args) <> ")"
  show (T.TyShift ty _) = "{" <> show ty <> "}"
  show (T.TyCo ty _) = "co " <> show ty

instance Show T.Decl where 
  show T.MkDataDecl{T.declNm=nm, T.declArgs=args, T.declPol=pl, T.declSig=sig} = 
    "data " <> nm <> "(" <> intercalate ", " ((\(v,p) -> v <> ":" <> show p) <$> args) <> ") :" <> show pl <> " where { " <> show sig <> "}"
  show T.MkValDecl{T.valVar = v, T.valTy=ty, T.valBd=bd} = 
    "val " <> v <> ":" <> show ty <> " = " <> show bd
  show T.MkRecDecl{T.recVar = v, T.recTy=ty, T.recBd=bd} = "rec " <> v <> ": " <> show ty <> " = " <> show bd
  show T.MkEps = "epsilon"
  show (T.MkCoDecl d) = "co " <> show d


instance Show T.XtorSig where 
  show T.MkXtorSig{T.sigName = nm, T.sigArgs=args} = nm <> "(" <> intercalate ", " (show <$> args) <> ")"
