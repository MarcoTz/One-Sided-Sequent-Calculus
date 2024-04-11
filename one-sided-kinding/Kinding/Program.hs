module Kinding.Program where 

import Kinding.Definition
import Kinding.Terms
import Kinding.Types
import Syntax.Typed.Program  qualified as T
import Syntax.Kinded.Program qualified as K

import Control.Monad

kindProgram :: T.Program -> KindM K.Program
kindProgram (T.MkProgram mn decls vars recs main src) = do 
  decls' <- forM decls kindDeclaration
  vars' <- forM vars kindVariable
  recs' <- forM recs kindRecDecl
  main' <- case main of Nothing -> return Nothing; Just c -> Just <$> kindCommand c
  return $ K.MkProgram mn decls' vars' recs' main' src

kindDeclaration :: T.DataDecl -> KindM K.DataDecl
kindDeclaration (T.MkData loc tyn tyArgs isCo xtors) = do
  xtors' <- forM xtors kindXtorSig
  return $ K.MkData loc tyn tyArgs isCo xtors'

kindVariable :: T.VarDecl -> KindM K.VarDecl
kindVariable (T.MkVar loc nm ty bd) = do
  ty' <- kindType ty 
  bd' <- kindTerm bd
  return $ K.MkVar loc nm ty' bd'

kindRecDecl :: T.RecDecl -> KindM K.RecDecl
kindRecDecl (T.MkRec loc nm ty bd) = do
  ty' <- kindType ty 
  bd' <- kindTerm bd
  return $ K.MkRec loc nm ty' bd'

kindXtorSig :: T.XtorSig -> KindM K.XtorSig
kindXtorSig (T.MkXtorSig loc nm args) = do
  args' <- forM args kindType
  return $ K.MkXtorSig loc nm args'

