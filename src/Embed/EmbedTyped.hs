module Embed.EmbedTyped where 

import Embed.Definition
import Embed.EmbedDesugared () 
import Syntax.Typed.Terms        qualified as T 
import Syntax.Typed.Program      qualified as T 
import Syntax.Typed.Types        qualified as T
import Syntax.Desugared.Terms    qualified as D
import Syntax.Desugared.Program  qualified as D
import Syntax.Desugared.Types    qualified as D
import Syntax.Parsed.Terms       qualified as P 
import Syntax.Parsed.Program     qualified as P

instance Embed T.Term D.Term where 
  embed (T.Var v _) = D.Var v
  embed (T.Mu v c _) = D.Mu v (embed c)
  embed (T.Xtor nm args _) = D.Xtor nm (embed <$> args)
  embed (T.XCase pts _) = D.XCase (embed <$> pts)
  embed (T.Shift t _) = D.Shift (embed t)
  embed (T.Lam v c _) = D.Lam v (embed c)
instance Embed T.Term P.Term where 
  embed t = (embed :: D.Term -> P.Term) $  (embed :: T.Term -> D.Term) t 

instance Embed T.Pattern D.Pattern where 
  embed (T.MkPattern xt v cmd) = D.MkPattern xt v (embed cmd)
instance Embed T.Pattern P.Pattern where 
  embed t = (embed :: D.Pattern -> P.Pattern) $ (embed :: T.Pattern -> D.Pattern) t

instance Embed T.Command D.Command where 
  embed (T.Cut t pol s) = D.Cut (embed t) pol (embed s)
  embed T.Done = D.Done
instance Embed T.Command P.Command where 
  embed t = (embed :: D.Command -> P.Command) $ (embed :: T.Command -> D.Command) t

instance Embed T.DataDecl D.Decl where 
  embed (T.MkDataDecl nm vars pol sigs) = D.MkData nm vars pol (embed <$> sigs)
instance Embed T.DataDecl P.Decl where 
  embed t = (embed :: D.Decl -> P.Decl) $ (embed :: T.DataDecl -> D.Decl) t


instance Embed T.XtorSig D.XtorSig where 
  embed (T.MkXtorSig nm args) = D.MkXtorSig nm (embed <$> args)
instance Embed T.XtorSig P.XtorSig where 
  embed t = (embed :: D.XtorSig -> P.XtorSig) $ (embed :: T.XtorSig -> D.XtorSig) t

instance Embed T.TypeScheme D.TypeScheme where 
  embed (T.MkTypeScheme vars ty) = D.MkTypeScheme vars (embed ty)
instance Embed T.Ty D.Ty where 
  embed (T.TyVar v _) = D.TyVar v
  embed (T.TyDecl nm args _) = D.TyDecl nm (embed <$> args)
  embed (T.TyShift ty _)  = embed ty
  embed (T.TyCo ty _) = embed ty 
instance Embed T.Ty P.Ty where 
  embed t = (embed :: D.Ty -> P.Ty) $ (embed :: T.Ty -> D.Ty) t 

instance Embed T.VarDecl D.Decl where 
  embed (T.MkVarDecl var ty body) = D.MkVar var (Just $ embed ty) (embed body)
instance Embed T.VarDecl P.Decl where 
  embed t = (embed :: D.Decl -> P.Decl) $ (embed :: T.VarDecl -> D.Decl) t

instance Embed T.Program [D.Decl] where 
  embed (T.MkProgram decls vars) = (embed <$> decls) ++ (embed <$> vars)
instance Embed T.Program [P.Decl] where 
  embed t = (embed :: D.Decl -> P.Decl) <$> (embed :: T.Program -> [D.Decl]) t 
