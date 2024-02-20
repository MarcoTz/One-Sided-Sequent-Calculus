module Pretty.Program where 

import Untyped.Program qualified as S 
import Typed.Program qualified as T 
import EmbedTyped
import Pretty.Common ()
import Pretty.Types ()
import Pretty.Terms ()

import Data.List (intercalate)


instance Show S.XtorSig where 
  show S.MkXtorSig{S.sigName = nm, S.sigArgs = []} = nm
  show S.MkXtorSig{S.sigName = nm, S.sigArgs=args} = nm <> "(" <> intercalate ", " (show <$> args) <> ")"
instance Show T.XtorSig where 
  show t = show $ (embed::T.XtorSig -> S.XtorSig) t

instance Show S.DataDecl where 
  show S.MkDataDecl{S.declNm=nm, S.declArgs=args, S.declPol=pl, S.declSig=sig} = 
    "data " <> nm <> "(" <> intercalate ", " ((\(v,p) -> v <> ":" <> show p) <$> args) <> ") :" <> show pl <> " { " <> intercalate ", "  (show <$> sig) <> " }"
instance Show T.DataDecl where 
  show t = show $ (embed :: T.DataDecl -> S.DataDecl) t

instance Show S.VarDecl where 
  show (S.MkVarDecl n t) = "val " <> n <> " = " <> show t
instance Show T.VarDecl where 
  show t = show $ (embed :: T.VarDecl -> S.VarDecl) t 

instance Show S.Program where 
  show (S.MkProgram decls terms) = "Type Declarations \n\t" <> intercalate ",\n\t" (show <$> decls) <> "\n Term Declarations\n\t"  <> intercalate ",\n\t" (show <$> terms)
instance Show T.Program where 
  show t = show $ (embed :: T.Program -> S.Program) t


