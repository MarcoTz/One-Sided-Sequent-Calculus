module Pretty.Types where 

import Syntax.Parsed.Program qualified as S
import Syntax.Typed.Types qualified as T
import Pretty.Common ()

import Data.List (intercalate)

instance Show S.Ty where 
  show (S.TyVar v) = v 
  show (S.TyDecl nm args) = nm <> "(" <> intercalate ", " (show <$> args) <> ")"

instance Show T.Ty where 
  show (T.TyVar v knd ) = v  <> " : " <> show knd
  show (T.TyDecl nm args knd) = nm <> "(" <> intercalate ", " (show <$> args) <> ") : " <> show knd 
  show (T.TyShift ty knd) = "{" <> show ty <> "} : " <>  show knd
  show (T.TyCo ty _) = "co " <> show ty

instance Show T.Kind where 
  show (T.MkKind p) = show p
  show (T.MkKindVar kv) = kv
