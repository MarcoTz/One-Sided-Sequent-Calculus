module Pretty where 

import Untyped.Syntax qualified as S
import Untyped.Program qualified as S
import Typed.Syntax   qualified as T
import Typed.Types    qualified as T
import Typed.Program  qualified as T
import EmbedTyped
import Driver.Definition
import Common
import TypeInference.Definition

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
  show (S.Xtor xt []) = xt
  show (S.Xtor xt args) = xt <> "(" <> intercalate ", " (show <$> args) <> ")"
  show (S.XCase pts) = "case {" <>  intercalate ", " (show <$> pts) <> "}"
  show (S.Shift t) = "{" <> show t <> "}"
  show (S.Lam v cmd) = "Lambda {" <> v <> "}." <> show cmd
instance Show T.Term where 
  show t = show $ (embed :: T.Term -> S.Term) t 


instance Show S.Ty where 
  show (S.TyVar v) = v 
  show (S.TyDecl nm args) = nm <> "(" <> intercalate ", " (show <$> args) <> ")"

instance Show T.Ty where 
  show (T.TyVar v _) = v 
  show (T.TyDecl nm args _) = nm <> "(" <> intercalate ", " (show <$> args) <> ")"
  show (T.TyShift ty _) = "{" <> show ty <> "}"
  show (T.TyCo ty _) = "co " <> show ty

instance Show S.DataDecl where 
  show S.MkDataDecl{S.declNm=nm, S.declArgs=args, S.declPol=pl, S.declSig=sig} = 
    "data " <> nm <> "(" <> intercalate ", " ((\(v,p) -> v <> ":" <> show p) <$> args) <> ") :" <> show pl <> " { " <> intercalate ", "  (show <$> sig) <> "}"
instance Show T.DataDecl where 
  show t = show $ (embed :: T.DataDecl -> S.DataDecl) t

instance Show T.VarDecl where 
  show T.MkValDecl{T.valVar = v, T.valTy=ty, T.valBd=bd} = 
    "val " <> v <> ":" <> show ty <> " = " <> show bd
instance Show T.RecDecl where 
  show T.MkRecDecl{T.recVar = v, T.recTy=ty, T.recBd=bd} = "rec " <> v <> ": " <> show ty <> " = " <> show bd
instance Show T.Eps where
  show T.MkEps = "epsilon"
instance Show T.Codecl where 
  show (T.MkCo d) = "co " <> show d


instance Show S.XtorSig where 
  show S.MkXtorSig{S.sigName = nm, S.sigArgs=args} = nm <> "(" <> intercalate ", " (show <$> args) <> ")"
instance Show T.XtorSig where 
  show t = show $ (embed::T.XtorSig -> S.XtorSig) t

instance Show T.Kind where 
  show (T.MkKind p) = show p
  show (T.MkKindVar kv) = kv

instance Show Constraint where 
  show (MkTyEq ty1 ty2) = show ty1 <> " = " <> show ty2
  show (MkKindEq k1 k2) = show k1 <> " = " <> show k2 
  show (MkFlipEq k1 k2) = show k1 <> " != " <> show k2
  show (MkProdEq p k1 k2) = show p <> " * " <> show k1 <> " = " <> show k2

instance Show DriverState where 
  show (MkDriverState _ env) = show env
