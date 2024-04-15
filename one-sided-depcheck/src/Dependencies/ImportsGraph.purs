module Dependencies.ImportsGraph (
  depOrderModule
) where 

import Common (Modulename)
import Syntax.Parsed.Program (Program(..), Import(..))
import Dependencies.Graph (getVertexLabel, getEdgesStartingAt, getEndingVert)
import Dependencies.Definition (DepM, ensureAcyclic, addVertexM, addEdgeM, getVertexError)
import Dependencies.Errors (DepError(..))

import Prelude (discard, bind, (<$>), pure, ($), (<>))
import Data.Traversable (for)
import Data.List (List(..), concat, elem)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit,unit)
import Control.Monad (when)
import Control.Monad.State (get)
import Control.Monad.Except (throwError)

type DepModule a = DepM Modulename a

depOrderModule :: Program -> List Program -> DepModule (List Modulename)
depOrderModule p@(Program prog) imports = do
 let mn = prog.progName
 addDependencies p
 _ <- for imports addDependencies
 ensureAcyclic (ErrDuplModule mn)
 getImportOrder mn

addDependencies :: Program -> DepModule Unit
addDependencies (Program prog) = do
  v1 <- addVertexM prog.progName 
  _ <- for prog.progImports (\(Import imp) -> do 
    v2 <- addVertexM imp.importName
    addEdgeM (Tuple v1 v2)
    pure unit)
  pure unit

getImportOrder :: Modulename -> DepModule (List Modulename)
getImportOrder mn = do
  gr <- get
  vert <- getVertexError mn (ErrUndefinedModule mn)
  let deps = (\x -> getVertexLabel (getEndingVert x)) <$> getEdgesStartingAt vert gr
  case deps of 
    Nil -> pure (Cons mn Nil)
    deps' -> do
      orders <- concat <$> for deps' getImportOrder 
      when (mn `elem` orders) $ throwError (ErrDuplModule mn)
      pure $ orders <> (Cons mn Nil)
