module Pretty.Types where 

import Syntax.Parsed.Types    qualified as P
import Syntax.Desugared.Types qualified as D
import Syntax.Typed.Types     qualified as T
import Embed.Definition
import Embed.EmbedDesugared ()
import Embed.EmbedTyped ()
import Pretty.Common ()

import Data.List (intercalate)

instance Show P.Ty where 
  show (P.TyVar v) = show v 
  show (P.TyDecl nm []) = show nm
  show (P.TyDecl nm args) = show nm <> "(" <> intercalate ", " (show <$> args) <> ")"
  show (P.TyCo ty) = "co " <> show ty
  show (P.TyForall args ty) = "forall " <> intercalate ", " (show <$> args) <> ". " <> show ty

--  show (P.TyForall vars ty) = "forall " <> intercalate ", " (show <$> vars) <> ". " <> show ty 
instance Show D.Ty where 
  show = show . (embed :: D.Ty -> P.Ty)

instance Show T.Ty where 
  show = show . (embed :: T.Ty -> P.Ty)
