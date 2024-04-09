module Embed.EmbedDesugared () where 

import Embed.Definition
import Common
import Syntax.Desugared.Terms    qualified as D
import Syntax.Desugared.Program  qualified as D
import Syntax.Desugared.Types    qualified as D
import Syntax.Parsed.Terms       qualified as P
import Syntax.Parsed.Program     qualified as P
import Syntax.Parsed.Types       qualified as P

import Data.Map qualified as M

instance Embed D.Term P.Term where 
  embed (D.Var loc v) = P.Var loc v
  embed (D.Mu loc v c) = P.Mu loc v (embed c)
  embed (D.Xtor loc nm args) = P.Xtor loc nm (embed <$> args)
  embed (D.XCase loc pts) = P.XCase loc (embed <$> pts)
  embed (D.ShiftCBV loc t) = P.ShiftCBV loc (embed t)
  embed (D.ShiftCBN loc t) = P.ShiftCBN loc (embed t)

instance Embed D.Pattern P.Pattern where 
  embed (D.MkPattern xt v cmd) = P.MkPattern xt v (embed cmd)

instance Embed D.Command P.Command where 
  embed (D.Cut loc t pol u) = P.Cut loc (embed t) pol (embed u)
  embed (D.CutAnnot loc t ty pol u) = P.CutAnnot loc (embed t) (embed ty) pol (embed u)
  embed (D.Done loc) = P.Done loc
  embed (D.Err loc err) = P.Err loc err
  embed (D.Print loc t) = P.Print loc (embed t)
  embed (D.PrintAnnot loc t ty) = P.PrintAnnot loc (embed t) (embed ty)

instance Embed D.DataDecl P.DataDecl where 
  embed (D.MkData loc nm vars pol sigs) = P.MkData loc nm vars pol (embed <$> sigs) 

instance Embed D.VarDecl P.VarDecl where 
  embed (D.MkVar loc var _ body) = P.MkVar loc var (embed body)

instance Embed D.RecDecl P.RecDecl where 
  embed (D.MkRec loc var _ body) = P.MkRec loc var (embed body)

instance Embed D.Program P.Program where 
  embed (D.MkProgram nm decls vars recs main src) = 
    P.MkProgram nm (M.map embed decls) (M.map embed vars) (M.map embed recs) (M.fromList . embedAnnots . M.toList $ vars) [] (embed <$> main) src
    where 
      embedAnnots :: [(Variable,D.VarDecl)] -> [(Variable,P.AnnotDecl)]
      embedAnnots [] = [] 
      embedAnnots ((_,D.MkVar _ _ Nothing _):ds) = embedAnnots ds
      embedAnnots ((_,D.MkVar loc v (Just ty) _):ds) = (v,P.MkAnnot loc v (embed ty)):embedAnnots ds

instance Embed D.XtorSig P.XtorSig where 
  embed (D.MkXtorSig loc nm args) = P.MkXtorSig loc nm (embed <$> args)

instance Embed D.KindedTy P.KindedTy  where 
  embed (D.KindedTy ty pol) = P.KindedTy (embed ty) pol 
instance Embed D.KindedTy P.Ty where 
  embed (D.KindedTy ty _) = embed ty
instance Embed D.Ty P.Ty where 
  embed (D.TyVar v) = P.TyVar v 
  embed (D.TyDecl nm args) = P.TyDecl nm (embed <$> args)
  embed (D.TyCo ty) = P.TyCo (embed ty)
  embed (D.TyShift ty) = P.TyShift (embed ty)
  embed (D.TyForall vars ty) = P.TyForall vars (embed ty)
