module Pretty.Types where 

import Syntax.Parsed.Types    qualified as P
import Syntax.Desugared.Types qualified as D
import Syntax.Typed.Types     qualified as T
import Embed.Definition
import Embed.EmbedDesugared ()
import Embed.EmbedTyped ()
import Pretty.Common ()
import Pretty.Parsed ()

instance Show D.PolTy where 
  show = show . (embed :: D.PolTy -> P.PolTy)

instance Show D.Ty where 
  show = show . (embed :: D.Ty -> P.Ty)
instance Show T.Ty where 
  show = show . (embed :: T.Ty->P.PolTy)
