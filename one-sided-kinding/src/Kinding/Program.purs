module Kinding.Program (
 kindProgram,
 kindVariable,
 kindDeclaration
)where 

import Kinding.Definition (KindM)
import Kinding.Terms (kindTerm,kindCommand)
import Kinding.Types (kindType)
import Syntax.Typed.Program (Program(..),DataDecl(..),VarDecl(..),XtorSig(..)) as T
import Syntax.Typed.Terms (Command) as T
import Syntax.Kinded.Program (Program(..),DataDecl(..),VarDecl(..),XtorSig(..)) as K
import Syntax.Kinded.Terms (Command) as K

import Prelude(pure, bind, ($))
import Data.Traversable (for)
import Data.Maybe (Maybe(..))


kindProgram :: T.Program -> KindM K.Program
kindProgram (T.Program prog) = do 
  decls' <- for prog.progDecls kindDeclaration
  vars' <- for prog.progVars kindVariable
  main' <- getMain prog.progMain 
  pure $ K.Program {progName:prog.progName, progDecls:decls', progVars:vars', progMain:main', progSrc:prog.progSrc}
  where 
    getMain :: Maybe T.Command -> KindM (Maybe K.Command)
    getMain Nothing = pure Nothing
    getMain (Just main) = do  
      main' <- kindCommand main
      pure (Just main')

kindDeclaration :: T.DataDecl -> KindM K.DataDecl
kindDeclaration (T.DataDecl decl) = do
  xtors' <- for decl.declXtors kindXtorSig
  pure $ K.DataDecl decl{declXtors=xtors'} 

kindVariable :: T.VarDecl -> KindM K.VarDecl
kindVariable (T.VarDecl var) = do
  ty' <- kindType var.varTy
  bd' <- kindTerm var.varBody
  pure $ K.VarDecl var{varBody=bd',varTy=ty'} 

kindXtorSig :: T.XtorSig -> KindM K.XtorSig
kindXtorSig (T.XtorSig sig) = do
  args' <- for sig.sigArgs kindType
  pure $ K.XtorSig {sigPos:sig.sigPos,sigName:sig.sigName, sigArgs:args'}  

