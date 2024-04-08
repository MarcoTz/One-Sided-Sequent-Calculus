module Pretty.Desugared () where 

import Syntax.Desugared.Program qualified as D 
import Syntax.Desugared.Terms qualified as D
import Syntax.Desugared.Types qualified as D
import Syntax.Parsed.Program qualified as P
import Syntax.Parsed.Types qualified as P
import Syntax.Parsed.Terms qualified as P
import Embed.Definition
import Embed.EmbedDesugared ()
import Pretty.Parsed ()

instance Show D.XtorSig where 
  show = show . (embed::D.XtorSig -> P.XtorSig)
instance Show D.DataDecl where 
  show = show . (embed::D.DataDecl->P.DataDecl)
instance Show D.VarDecl where 
  show = show . (embed::D.VarDecl->P.VarDecl)
instance Show D.RecDecl where 
  show = show . (embed::D.RecDecl->P.RecDecl)
instance Show D.Program where 
  show = show . (embed::D.Program->P.Program)

instance Show D.KindedTy where 
  show = show . (embed :: D.KindedTy-> P.KindedTy)
instance Show D.Ty where 
  show = show . (embed :: D.Ty -> P.Ty)

instance Show D.Term where 
  show = show . (embed :: D.Term -> P.Term)

instance Show D.Pattern where
  show = show . (embed :: D.Pattern -> P.Pattern)

instance Show D.Command where 
  show = show . (embed :: D.Command -> P.Command)
