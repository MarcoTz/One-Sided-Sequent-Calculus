module Pretty.Program where 

import Syntax.Untyped.Program qualified as S 
import Syntax.Typed.Program qualified as T 
import EmbedTyped
import Pretty.Common ()
import Pretty.Types ()
import Pretty.Terms ()

import Data.List (intercalate)


instance Show S.XtorSig where 
  show S.MkXtorSig{S.sigName = nm, S.sigArgs = []} = nm
  show S.MkXtorSig{S.sigName = nm, S.sigArgs=args} = nm <> "(" <> intercalate ", " (show <$> args) <> ")"
instance Show T.XtorSig where 
  show (T.MkXtorSig n []) = n
  show (T.MkXtorSig n args) = n <> "(" <> intercalate ", " (show <$> args) <> ")"

instance Show S.DataDecl where 
  show (S.MkDataDecl n args pol sigs) =  
    "data " <> n <> "(" <> intercalate ", " ((\(v,p) -> v <> show p) <$> args) <> ") : " <> show pol <> " {" <> intercalate ",\n\t " (show <$> sigs) <> "}"
instance Show T.DataDecl where 
  show (T.MkDataDecl n vars pol sigs) = 
    "data " <> n <> "(" <> intercalate ", " ((\(v,p) -> v <> show p) <$> vars) <> ") : " <> show pol <> " {" <> intercalate ",\n\t" (show <$> sigs) <> "}"

instance Show S.VarDecl where 
  show (S.MkVarDecl n t) = "val " <> n <> " = " <> show t
instance Show T.VarDecl where 
  show t = show $ (embed :: T.VarDecl -> S.VarDecl) t 

instance Show S.Program where 
  show (S.MkProgram decls terms) = "Type Declarations \n\t" <> intercalate ",\n\t" (show <$> decls) <> "\n Term Declarations\n\t"  <> intercalate ",\n\t" (show <$> terms)
instance Show T.Program where 
  show t = show $ (embed :: T.Program -> S.Program) t


