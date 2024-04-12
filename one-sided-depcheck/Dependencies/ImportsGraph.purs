module Dependencies.ImportsGraph (
  depOrderModule
) where 

import Dependencies.Definition
import Dependencies.Graph
import Syntax.Parsed.Program
import Common

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

type DepModule a = DepM Modulename a

depOrderModule :: Program -> [Program] -> DepModule [Modulename]
depOrderModule prog imports = do
 let mn = progName prog
 addDependencies prog
 forM_ imports addDependencies
 ensureAcyclic (ErrDuplModule mn)
 getImportOrder mn

addDependencies :: Program -> DepModule ()
addDependencies (MkProgram mn _ _ _ _ imports _ _) = do
  v1 <- addVertexM mn 
  forM_ imports (\(MkImport _ mn') -> do 
    v2 <- addVertexM mn'
    addEdgeM (v1,v2))

getImportOrder :: Modulename -> DepModule [Modulename]
getImportOrder mn = do
  gr <- get
  vert <- getVertexError mn (ErrUndefinedModule mn)
  let deps = getVertexLabel . getEndingVert <$> getEdgesStartingAt vert gr
  case deps of 
    [] -> return [mn]
    deps' -> do
      orders <- concat <$> forM deps' getImportOrder 
      when (mn `elem` orders) $ throwError (ErrDuplModule mn)
      return $ orders ++ [mn] 
