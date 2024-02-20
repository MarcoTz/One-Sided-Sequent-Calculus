module Pretty.Program where 

import Untyped.Program qualified as S 
import Typed.Program qualified as T 
import EmbedTyped
import Pretty.Common ()
import Pretty.Types ()
import Pretty.Terms ()

import Data.List (intercalate)


instance Show S.XtorSig where 
  show S.MkXtorSig{S.sigName = nm, S.sigArgs=args} = nm <> "(" <> intercalate ", " (show <$> args) <> ")"
instance Show T.XtorSig where 
  show t = show $ (embed::T.XtorSig -> S.XtorSig) t

instance Show S.DataDecl where 
  show S.MkDataDecl{S.declNm=nm, S.declArgs=args, S.declPol=pl, S.declSig=sig} = 
    "data " <> nm <> "(" <> intercalate ", " ((\(v,p) -> v <> ":" <> show p) <$> args) <> ") :" <> show pl <> " { " <> intercalate ", "  (show <$> sig) <> "}"
instance Show T.DataDecl where 
  show t = show $ (embed :: T.DataDecl -> S.DataDecl) t

instance Show S.TermDecl where 
  show (S.MkTermDecl n t) = "val " <> n <> " = " <> show t

instance Show S.Program where 
  show (S.MkProgram decls terms) = "Type Declarations " <> intercalate ", " (show <$> decls) <> "\n Term Declarations"  <> intercalate ", " (show <$> terms)


instance Show T.VarDecl where 
  show T.MkValDecl{T.valVar = v, T.valTy=ty, T.valBd=bd} = 
    "val " <> v <> ":" <> show ty <> " = " <> show bd
instance Show T.RecDecl where 
  show T.MkRecDecl{T.recVar = v, T.recTy=ty, T.recBd=bd} = "rec " <> v <> ": " <> show ty <> " = " <> show bd
instance Show T.Eps where
  show T.MkEps = "epsilon"
instance Show T.Codecl where 
  show (T.MkCo d) = "co " <> show d

