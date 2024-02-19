module EmbedTyped where 

import Typed.Syntax qualified as T 
import Untyped.Syntax qualified as S

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
