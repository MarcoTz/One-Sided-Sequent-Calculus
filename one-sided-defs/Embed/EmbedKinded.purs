module Embed.EmbedKinded where 


import Embed.Definition
import Embed.EmbedDesugared ()
import Embed.EmbedTyped ()
import Common
import Syntax.Kinded.Terms       qualified as K
import Syntax.Kinded.Program     qualified as K
import Syntax.Kinded.Types       qualified as K
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

-----------------------------------------------------------------
----------------------------- Terms -----------------------------
-----------------------------------------------------------------
instance Embed K.Term T.Term where 
  embed (K.Var loc v ty) = T.Var loc v (embed ty)
  embed (K.Mu loc v c ty) = T.Mu loc v (embed c) (embed ty)
  embed (K.Xtor loc nm args ty) = T.Xtor loc nm (embed <$> args) (embed ty)
  embed (K.XCase loc pts ty) = T.XCase loc (embed <$> pts) (embed ty)
  embed (K.ShiftCBV loc t ty) = T.ShiftCBV loc (embed t) (embed ty)
  embed (K.ShiftCBN loc t ty) = T.ShiftCBN loc (embed t) (embed ty)
instance Embed K.Term D.Term where 
  embed t = (embed :: T.Term -> D.Term) $ (embed :: K.Term -> T.Term) t
instance Embed K.Term P.Term where 
  embed t = (embed :: D.Term -> P.Term) $  (embed :: K.Term -> D.Term) t 

instance Embed K.Pattern T.Pattern where 
  embed (K.MkPattern xt v cmd) = T.MkPattern xt v (embed cmd)
instance Embed K.Pattern D.Pattern where 
  embed t = (embed :: T.Pattern -> D.Pattern) $ (embed :: K.Pattern -> T.Pattern) t
instance Embed K.Pattern P.Pattern where 
  embed t = (embed :: D.Pattern -> P.Pattern) $ (embed :: K.Pattern -> D.Pattern) t

instance Embed K.Command T.Command where 
  embed (K.Cut loc t eo s) = T.Cut loc (embed t) eo (embed s)
  embed (K.Done loc) = T.Done loc
  embed (K.Err loc err) = T.Err loc err
  embed (K.Print loc t) = T.Print loc (embed t) 
instance Embed K.Command D.Command where 
  embed t = (embed :: T.Command -> D.Command) $ (embed :: K.Command -> T.Command) t
instance Embed K.Command P.Command where 
  embed t = (embed :: D.Command -> P.Command) $ (embed :: K.Command -> D.Command) t

-------------------------------------------------------------------
----------------------------- Program -----------------------------
-------------------------------------------------------------------

instance Embed K.DataDecl T.DataDecl where 
  embed (K.MkData loc nm vars pol sigs) = T.MkData loc nm vars pol (embed <$> sigs)
instance Embed K.DataDecl D.DataDecl where 
  embed t = (embed :: T.DataDecl -> D.DataDecl) $ (embed :: K.DataDecl -> T.DataDecl) t
instance Embed K.DataDecl P.DataDecl where 
  embed t = (embed :: D.DataDecl -> P.DataDecl) $ (embed :: K.DataDecl -> D.DataDecl) t

instance Embed K.XtorSig T.XtorSig where 
  embed (K.MkXtorSig loc nm args) = T.MkXtorSig loc nm (embed <$> args)
instance Embed K.XtorSig D.XtorSig where 
  embed t = (embed :: T.XtorSig -> D.XtorSig) $ (embed :: K.XtorSig -> T.XtorSig) t
instance Embed K.XtorSig P.XtorSig where 
  embed t = (embed :: D.XtorSig -> P.XtorSig) $ (embed :: K.XtorSig -> D.XtorSig) t


-------------------------------------------------------------------
----------------------------- Types -------------------------------
-------------------------------------------------------------------


instance Embed K.Ty T.KindedTy where 
  embed ty = T.KindedTy (embed ty) (getKind ty)
instance Embed K.Ty D.KindedTy where 
  embed ty = (embed :: T.KindedTy -> D.KindedTy) $ (embed :: K.Ty -> T.KindedTy) ty 
instance Embed K.Ty P.KindedTy where 
  embed ty = (embed :: T.KindedTy -> P.KindedTy) $ (embed :: K.Ty -> T.KindedTy) ty 

instance Embed K.Ty T.Ty where 
  embed (K.TyVar v _) = T.TyVar v
  embed (K.TyDecl nm args _) = T.TyDecl nm (embed <$> args)
  embed (K.TyShift ty _)  = T.TyShift (embed ty)
  embed (K.TyCo ty) = T.TyCo (embed ty)
  embed (K.TyForall args ty) = T.TyForall args (embed ty)

instance Embed K.Ty D.Ty where 
  embed t = (embed :: T.Ty -> D.Ty) $ (embed :: K.Ty -> T.Ty) t 
instance Embed K.Ty P.Ty where 
  embed t = (embed :: D.Ty -> P.Ty) $ (embed :: K.Ty -> D.Ty) t 

instance Embed K.VarDecl T.VarDecl where 
  embed (K.MkVar loc var ty body) = T.MkVar loc var (embed ty) (embed body)
instance Embed K.VarDecl D.VarDecl where 
  embed t = (embed :: T.VarDecl -> D.VarDecl) $ (embed :: K.VarDecl -> T.VarDecl) t
instance Embed K.VarDecl P.VarDecl where 
  embed t = (embed :: D.VarDecl -> P.VarDecl) $ (embed :: K.VarDecl -> D.VarDecl) t

instance Embed K.RecDecl T.RecDecl where 
  embed (K.MkRec loc var ty body) = T.MkRec loc var (embed ty) (embed body)
instance Embed K.RecDecl D.RecDecl where 
  embed t = (embed :: T.RecDecl -> D.RecDecl) $ (embed :: K.RecDecl -> T.RecDecl) t
instance Embed K.RecDecl P.RecDecl where 
  embed t = (embed :: D.RecDecl -> P.RecDecl) $ (embed :: K.RecDecl -> D.RecDecl) t

instance Embed K.Program T.Program where 
  embed (K.MkProgram nm decls vars recs main src) = 
    T.MkProgram nm (M.map embed decls) (M.map embed vars) (M.map embed recs) (embed <$> main) src
instance Embed K.Program D.Program where 
  embed t = (embed :: T.Program -> D.Program) $ (embed :: K.Program -> T.Program) t 
instance Embed K.Program P.Program where 
  embed t = (embed :: D.Program -> P.Program) $ (embed :: K.Program -> D.Program) t 
