module Embed.EmbedDesugared where 

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
  embed (D.Var v) = P.Var v
  embed (D.Mu v c) = P.Mu v (embed c)
  embed (D.Xtor nm args) = P.Xtor nm (embed <$> args)
  embed (D.XCase pts) = P.XCase (embed <$> pts)
  embed (D.Shift t) = P.Shift (embed t)
  embed (D.Lam v c) = P.Lam v (embed c)

instance Embed D.Pattern P.Pattern where 
  embed (D.MkPattern xt v cmd) = P.MkPattern xt v (embed cmd)

instance Embed D.Command P.Command where 
  embed (D.Cut t pol s) = P.Cut (embed t) pol (embed s)
  embed D.Done = P.Done

instance Embed D.DataDecl P.DataDecl where 
  embed (D.MkData nm vars pol sigs) = P.MkData nm vars pol (embed <$> sigs) 

instance Embed D.VarDecl P.VarDecl where 
  embed (D.MkVar var _ body) = P.MkVar var (embed body)

instance Embed D.Program P.Program where 
  embed (D.MkProgram decls vars) = P.MkProgram (M.map embed decls) (M.map embed vars) (M.fromList . embedAnnots . M.toList $ vars)
    where 
      embedAnnots :: [(Variable,D.VarDecl)] -> [(Variable,P.AnnotDecl)]
      embedAnnots [] = [] 
      embedAnnots ((_,D.MkVar _ Nothing _):ds) = embedAnnots ds
      embedAnnots ((_,D.MkVar v (Just ty) _):ds) = (v,P.MkAnnot v (embed ty)):embedAnnots ds

instance Embed D.XtorSig P.XtorSig where 
  embed (D.MkXtorSig nm args) = P.MkXtorSig nm (embed <$> args)

instance Embed D.Ty P.Ty where 
  embed (D.TyVar v) = P.TyVar v 
  embed (D.TyDecl nm args) = P.TyDecl nm (embed <$> args)
  embed (D.TyForall vars ty) = P.TyForall vars (embed ty)

