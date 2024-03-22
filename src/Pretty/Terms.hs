module Pretty.Terms where 

import Syntax.Parsed.Terms qualified as P 
import Syntax.Desugared.Terms qualified as D
import Syntax.Typed.Terms qualified as T 
import Embed.Definition
import Embed.EmbedDesugared () 
import Embed.EmbedTyped () 
import Pretty.Parsed ()
import Pretty.Common ()
import Pretty.Types ()

instance Show D.Term where 
  show = show . (embed :: D.Term -> P.Term)
instance Show T.Term where 
  show = show . (embed :: T.Term -> P.Term)

instance Show D.Pattern where
  show = show . (embed :: D.Pattern -> P.Pattern)
instance Show T.Pattern where 
  show = show . (embed :: T.Pattern -> P.Pattern)

instance Show D.Command where 
  show = show . (embed :: D.Command -> P.Command)
instance Show T.Command where 
  show = show . (embed :: T.Command -> P.Command)
