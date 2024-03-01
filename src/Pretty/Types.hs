module Pretty.Types where 

import Syntax.Parsed.Program qualified as P
import Syntax.Desugared.Types qualified as D
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
  show (T.TyVar v knd) = v <> ":" <> show knd
  show (T.TyDecl nm [] knd) = nm <> ":" <> show knd
  show (T.TyDecl nm args knd) = nm <> "(" <> intercalate ", " (show <$> args) <> ") :" <> show knd
  show (T.TyShift ty knd) = "{" <> show ty <> "}: " <> show knd
  show (T.TyCo ty knd) = "co " <> show ty <> ":" <> show knd

instance Show P.TypeScheme where 
  show (P.MkTypeScheme [] ty) = show ty
  show (P.MkTypeScheme vars ty) = "forall " <> intercalate "," vars <> ". " <> show ty

instance Show D.TypeScheme where 
  show t = show $ (embed :: D.TypeScheme -> P.TypeScheme) t
instance Show T.TypeScheme where 
  show (T.MkTypeScheme [] ty) = show ty
  show (T.MkTypeScheme vars ty) = "forall " <> intercalate "," vars <> ". " <> show ty
