module Pretty.Program where 

import Syntax.Parsed.Program    qualified as P
import Syntax.Desugared.Program qualified as D
import Syntax.Typed.Program     qualified as T
import Embed.Definition
import Embed.EmbedTyped  () 
import Embed.EmbedDesugared () 
import Pretty.Common ()
import Pretty.Parsed ()
import Pretty.Types ()
import Pretty.Terms ()

instance Show D.XtorSig where 
  show = show . (embed :: D.XtorSig -> P.XtorSig)
instance Show T.XtorSig where
  show = show . (embed :: T.XtorSig -> P.XtorSig)

instance Show D.DataDecl where 
  show = show . (embed :: D.DataDecl -> P.DataDecl)
instance Show T.DataDecl where 
  show = show . (embed :: T.DataDecl -> P.DataDecl)

instance Show D.VarDecl where 
  show = show . (embed :: D.VarDecl -> P.VarDecl) 
instance Show T.VarDecl where 
  show = show . (embed :: T.VarDecl -> P.VarDecl)

instance Show D.RecDecl where 
  show = show . (embed :: D.RecDecl -> P.RecDecl)
instance Show T.RecDecl where 
  show = show . (embed :: T.RecDecl -> P.RecDecl)

instance Show D.Program where 
  show = show . (embed :: D.Program -> P.Program)
instance Show T.Program where 
  show = show . (embed :: T.Program -> P.Program)
