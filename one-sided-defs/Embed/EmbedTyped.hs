module Embed.EmbedTyped () where 

import Embed.Definition
import Embed.EmbedDesugared () 
import Common
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
  embed (T.Var loc v _) = D.Var loc v
  embed (T.Mu loc v c _) = D.Mu loc v (embed c)
  embed (T.Xtor loc nm args _) = D.Xtor loc nm (embed <$> args)
  embed (T.XCase loc pts _) = D.XCase loc (embed <$> pts)
  embed (T.ShiftCBV loc t _) = D.ShiftCBV loc (embed t)
  embed (T.ShiftCBN loc t _) = D.ShiftCBN loc (embed t)
instance Embed T.Term P.Term where 
  embed t = (embed :: D.Term -> P.Term) $  (embed :: T.Term -> D.Term) t 

instance Embed T.Pattern D.Pattern where 
  embed (T.MkPattern xt v cmd) = D.MkPattern xt v (embed cmd)
instance Embed T.Pattern P.Pattern where 
  embed t = (embed :: D.Pattern -> P.Pattern) $ (embed :: T.Pattern -> D.Pattern) t

instance Embed T.Command D.Command where 
  embed (T.Cut loc t pol s) = D.CutAnnot loc (embed t) (embed $ T.getType t) pol (embed s)
  embed (T.Done loc) = D.Done loc
  embed (T.Err loc err) = D.Err loc err
  embed (T.Print loc t) = D.PrintAnnot loc (embed t) (embed . T.getType $ t)
instance Embed T.Command P.Command where 
  embed t = (embed :: D.Command -> P.Command) $ (embed :: T.Command -> D.Command) t

instance Embed T.DataDecl D.DataDecl where 
  embed (T.MkData loc nm vars pol sigs) = D.MkData loc nm vars pol (embed <$> sigs)
instance Embed T.DataDecl P.DataDecl where 
  embed t = (embed :: D.DataDecl -> P.DataDecl) $ (embed :: T.DataDecl -> D.DataDecl) t

instance Embed T.XtorSig D.XtorSig where 
  embed (T.MkXtorSig loc nm args) = D.MkXtorSig loc nm (embed <$> args)
instance Embed T.XtorSig P.XtorSig where 
  embed t = (embed :: D.XtorSig -> P.XtorSig) $ (embed :: T.XtorSig -> D.XtorSig) t

instance Embed T.Ty D.Ty where 
  embed (T.TyVar v _) = D.TyVar v
  embed (T.TyDecl nm args _) = D.TyDecl nm (embed <$> args)
  embed (T.TyShift ty _)  = embed ty
  embed (T.TyCo ty) = D.TyCo (embed ty)
  embed (T.TyForall args ty) = D.TyForall args (embed ty)
instance Embed T.Ty P.Ty where 
  embed t = (embed :: D.Ty -> P.Ty) $ (embed :: T.Ty -> D.Ty) t 

instance Embed T.Ty D.KindedTy where 
  embed ty = D.KindedTy (embed ty) (getKind ty)
instance Embed T.Ty P.KindedTy where 
  embed = (embed :: D.KindedTy -> P.KindedTy) . (embed  :: T.Ty -> D.KindedTy)

instance Embed T.VarDecl D.VarDecl where 
  embed (T.MkVar loc var ty body) = D.MkVar loc var (Just (embed ty)) (embed body)
instance Embed T.VarDecl P.VarDecl where 
  embed t = (embed :: D.VarDecl -> P.VarDecl) $ (embed :: T.VarDecl -> D.VarDecl) t

instance Embed T.RecDecl D.RecDecl where 
  embed (T.MkRec loc var ty body) = D.MkRec loc var (Just (embed ty)) (embed body)
instance Embed T.RecDecl P.RecDecl where 
  embed t = (embed :: D.RecDecl -> P.RecDecl) $ (embed :: T.RecDecl -> D.RecDecl) t

instance Embed T.Program D.Program where 
  embed (T.MkProgram nm decls vars recs main src) = 
    D.MkProgram nm (M.map embed decls) (M.map embed vars) (M.map embed recs) (embed <$> main) src
instance Embed T.Program P.Program where 
  embed t = (embed :: D.Program -> P.Program) $ (embed :: T.Program -> D.Program) t 
