module Pretty.Types where 

import Syntax.Parsed.Program qualified as P
import Syntax.Desugared.Program qualified as D
import Syntax.Typed.Types qualified as T
import Embed.Definition
import Embed.EmbedDesugared ()
import Pretty.Common ()

import Data.List (intercalate)

instance Show P.Ty where 
  show (P.TyVar v) = v 
  show (P.TyDecl nm []) = nm
  show (P.TyDecl nm args) = nm <> "(" <> intercalate ", " (show <$> args) <> ")"
instance Show D.Ty where 
  show = show . (embed :: D.Ty -> P.Ty)

instance Show T.Ty where 
  show (T.TyVar v knd ) = v  <> " : " <> show knd
  show (T.TyDecl nm [] knd) = nm <> ":" <> show knd
  show (T.TyDecl nm args knd) = nm <> "(" <> intercalate ", " (show <$> args) <> ") : " <> show knd 
  show (T.TyShift ty knd) = "{" <> show ty <> "} : " <>  show knd
  show (T.TyCo ty _) = "co " <> show ty

instance Show T.Kind where 
  show (T.MkKind p) = show p
  show (T.MkKindVar kv) = kv
