module Pretty.Kinded where 

import Syntax.Parsed.Types     qualified as P
import Syntax.Parsed.Program   qualified as P
import Syntax.Parsed.Terms     qualified as P
import Syntax.Kinded.Types     qualified as K
import Syntax.Kinded.Program   qualified as K
import Syntax.Kinded.Terms     qualified as K
import Embed.Definition
import Embed.EmbedDesugared ()
import Embed.EmbedKinded ()
import Pretty.Common ()
import Pretty.Parsed ()

instance Show K.Ty where 
  show = show . (embed :: K.Ty->P.KindedTy)
instance Show K.XtorSig where
  show = show . (embed :: K.XtorSig -> P.XtorSig)
instance Show K.DataDecl where 
  show = show . (embed :: K.DataDecl -> P.DataDecl)
instance Show K.VarDecl where 
  show = show . (embed :: K.VarDecl -> P.VarDecl)
instance Show K.RecDecl where 
  show = show . (embed :: K.RecDecl -> P.RecDecl)
instance Show K.Program where 
  show = show . (embed :: K.Program -> P.Program)
instance Show K.Term where 
  show = show . (embed :: K.Term -> P.Term)
instance Show K.Pattern where 
  show = show . (embed :: K.Pattern -> P.Pattern)
instance Show K.Command where 
  show = show . (embed :: K.Command -> P.Command)
