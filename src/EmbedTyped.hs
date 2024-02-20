module EmbedTyped where 

import Typed.Syntax    qualified as T 
import Typed.Program   qualified as T 
import Typed.Types     qualified as T
import Untyped.Syntax  qualified as S
import Untyped.Program qualified as S

class Embed a b where 
  embed :: a -> b 

instance Embed T.Term S.Term where 
  embed (T.Var v _) = S.Var v
  embed (T.Mu v c _) = S.Mu v (embed c)
  embed (T.Xtor nm args _) = S.Xtor nm (embed <$> args)
  embed (T.XCase pts _) = S.XCase (embed <$> pts)
  embed (T.Shift t _) = S.Shift (embed t)
  embed (T.Lam v c _) = S.Lam v (embed c)

instance Embed T.Pattern S.Pattern where 
  embed (T.MkPattern xt v cmd) = S.MkPattern xt v (embed cmd)

instance Embed T.Command S.Command where 
  embed (T.Cut t pol s) = S.Cut (embed t) pol (embed s)
  embed T.Done = S.Done

instance Embed T.DataDecl S.DataDecl where 
  embed (T.MkDataDecl nm vars pol sigs) = S.MkDataDecl nm vars pol (embed <$> sigs)

instance Embed T.XtorSig S.XtorSig where 
  embed (T.MkXtorSig nm args) = S.MkXtorSig nm (embed <$> args)

instance Embed T.Ty S.Ty where 
  embed (T.TyVar v _) = S.TyVar v 
  embed (T.TyDecl nm args _) = S.TyDecl nm (embed <$> args)
  embed (T.TyShift ty _)  = embed ty 
  embed (T.TyCo ty _) = embed ty 


