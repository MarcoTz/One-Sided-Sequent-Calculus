module Pretty.Typed () where 

import Syntax.Parsed.Types    qualified as P
import Syntax.Parsed.Program  qualified as P
import Syntax.Parsed.Terms    qualified as P
import Syntax.Typed.Types     qualified as T
import Syntax.Typed.Program   qualified as T
import Syntax.Typed.Terms     qualified as T
import Embed.Definition
import Embed.EmbedDesugared ()
import Embed.EmbedTyped ()
import Pretty.Common ()
import Pretty.Parsed ()

instance Show T.Ty where 
  show = show . (embed :: T.Ty->P.KindedTy)
instance Show T.XtorSig where
  show = show . (embed :: T.XtorSig -> P.XtorSig)
instance Show T.DataDecl where 
  show = show . (embed :: T.DataDecl -> P.DataDecl)
instance Show T.VarDecl where 
  show = show . (embed :: T.VarDecl -> P.VarDecl)
instance Show T.RecDecl where 
  show = show . (embed :: T.RecDecl -> P.RecDecl)
instance Show T.Program where 
  show = show . (embed :: T.Program -> P.Program)
instance Show T.Term where 
  show = show . (embed :: T.Term -> P.Term)
instance Show T.Pattern where 
  show = show . (embed :: T.Pattern -> P.Pattern)
instance Show T.Command where 
  show = show . (embed :: T.Command -> P.Command)
