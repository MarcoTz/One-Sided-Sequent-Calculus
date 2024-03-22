module Dependencies.ImportsGraph (
  depOrderModule
) where 

import Dependencies.Definition
import Dependencies.Graph
import Syntax.Parsed.Program
import Common
import Errors

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

type DepModule a = DepM Modulename a

depOrderModule :: Modulename -> [(Modulename,[Import])] -> DepModule [Modulename]
depOrderModule mn m = do
 _<-addVertexM mn
 createGraph m
 ensureAcyclic (ErrDuplModule mn "ensureAcyclic")
 getImportOrder mn

createGraph :: [(Modulename,[Import])] -> DepModule () 
createGraph m = forM_ m (uncurry addImport)

addImport :: Modulename -> [Import] -> DepModule ()
addImport mn imps = forM_ imps (\(MkImport mn') -> do
  v1 <- addVertexM mn
  v2 <- addVertexM mn'
  addEdgeM (v1,v2))

getImportOrder :: Modulename -> DepModule [Modulename]
getImportOrder mn = do
  gr <- get
  vert <- getVertexError mn (ErrModuleNotFound mn "dependency graph")
  let preds = getVertexLabel . getStartingVert <$> getEdgesEndingAt vert gr
  case preds of 
    [] -> return [mn]
    preds' -> do
      orders <- concat <$> forM preds' getImportOrder 
      when (mn `elem` orders) $ throwError (ErrDuplModule mn "getimportorder")
      return $ orders ++ [mn] 
