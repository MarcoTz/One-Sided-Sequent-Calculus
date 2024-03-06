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
import Syntax.Parsed.Types       qualified as P

import Data.Map qualified as M

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

instance Embed T.DataDecl D.DataDecl where 
  embed (T.MkDataDecl nm vars pol sigs) = D.MkData nm vars pol (embed <$> sigs)
instance Embed T.DataDecl P.DataDecl where 
  embed t = (embed :: D.DataDecl -> P.DataDecl) $ (embed :: T.DataDecl -> D.DataDecl) t

instance Embed T.XtorSig D.XtorSig where 
  embed (T.MkXtorSig nm args) = D.MkXtorSig nm (embed <$> args)
instance Embed T.XtorSig P.XtorSig where 
  embed t = (embed :: D.XtorSig -> P.XtorSig) $ (embed :: T.XtorSig -> D.XtorSig) t

instance Embed T.Ty D.Ty where 
  embed (T.TyVar v _) = D.TyVar v
  embed (T.TyDecl nm args _) = D.TyDecl nm (embed <$> args)
  embed (T.TyShift ty)  = embed ty
  embed (T.TyCo ty) = D.TyCo (embed ty)
--  embed (T.TyForall vars ty) = D.TyForall vars (embed ty)
instance Embed T.Ty P.Ty where 
  embed t = (embed :: D.Ty -> P.Ty) $ (embed :: T.Ty -> D.Ty) t 

instance Embed T.VarDecl D.VarDecl where 
  embed (T.MkVarDecl var ty body) = D.MkVar var (Just (embed ty)) (embed body)
instance Embed T.VarDecl P.VarDecl where 
  embed t = (embed :: D.VarDecl -> P.VarDecl) $ (embed :: T.VarDecl -> D.VarDecl) t

instance Embed T.Program D.Program where 
  embed (T.MkProgram nm decls vars) = D.MkProgram nm (M.map embed decls) (M.map embed vars) 
instance Embed T.Program P.Program where 
  embed t = (embed :: D.Program -> P.Program) $ (embed :: T.Program -> D.Program) t 
