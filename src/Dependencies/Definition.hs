module Dependencies.Definition where

import Errors 

import Data.Set qualified as S
import Data.Foldable (find)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Functor


data Vertex a = MkVertex !Int !a
  deriving (Eq)
instance Eq a => Ord (Vertex a) where 
  compare (MkVertex i _) (MkVertex j _) = compare i j
  (MkVertex i _) <= (MkVertex j _) = i <= j

data Edge   a = MkEdge   !(Vertex a) !(Vertex a)
  deriving (Eq) 
data Graph  a = MkGraph {grVerts :: !(S.Set (Vertex a)), grEdges :: ![Edge a]}

emptyGraph :: Graph a 
emptyGraph = MkGraph S.empty []

addVertex :: Eq a => a -> Graph a -> (Vertex a, Graph a)
addVertex v gr@(MkGraph verts edges) = 
  case getVertex v gr of 
    Just vert -> (vert,gr)
    Nothing ->  
      let n = length verts
          newVert = MkVertex (n+1) v in
      (newVert, MkGraph (S.insert newVert verts) edges)

getVertex :: Eq a => a -> Graph a -> Maybe (Vertex a)
getVertex lb (MkGraph verts _) = find (\(MkVertex _ lb') -> lb == lb') verts 

getVertexLabel :: Vertex a -> a 
getVertexLabel (MkVertex _ l) = l

addEdge :: Eq a => (Vertex a,Vertex a) -> Graph a -> Graph a
addEdge (a1,a2) (MkGraph verts edges) = MkGraph (S.insert a2 $ S.insert a1 verts)  (MkEdge a1 a2 : edges) 

getStartingVert :: Edge a -> Vertex a
getStartingVert (MkEdge v _) = v

getEndingVert :: Edge a -> Vertex a 
getEndingVert (MkEdge _ v) = v

getEdgesStartingAt :: Eq a => Vertex a -> Graph a -> [Edge a]
getEdgesStartingAt vert (MkGraph _ edges) = filter (\e -> getStartingVert e==vert) edges

getEdgesEndingAt :: Eq a => Vertex a -> Graph a -> [Edge a]
getEdgesEndingAt vert (MkGraph _ edges) = filter (\e -> getEndingVert e==vert) edges

newtype DepM a b = DepM { getCheckM :: (StateT (Graph a) (Except Error)) b }
  deriving newtype (Functor, Applicative, Monad, MonadError Error, MonadState (Graph a))

addVertexM :: Eq a => a -> DepM a (Vertex a)
addVertexM a = do
  gr <- get
  let (newV,gr') = addVertex a gr
  modify (const gr')
  return newV

getVertexError :: Eq a => a -> Error -> DepM a (Vertex a) 
getVertexError lb err = do 
  gr <- get 
  let mvert = getVertex lb gr 
  case mvert of 
    Nothing   -> throwError err 
    Just vert -> return vert

addEdgeM :: Eq a => (Vertex a, Vertex a) -> DepM a () 
addEdgeM e = modify (addEdge e)

ensureAcyclic :: Eq a => Vertex a -> Error -> DepM a () 
ensureAcyclic start err = traverseGraph start [] [] $> ()
  where 
    traverseGraph :: Eq a => Vertex a -> [Vertex a] -> [Edge a] -> DepM a ([Vertex a],[Edge a])
    traverseGraph startV seenV seenE = do 
      when (startV `elem` seenV) $ throwError err
      gr <- get 
      let outg = getEdgesEndingAt startV gr
      let outgF = filter (`notElem` seenE) outg
      let newSeenV = startV : seenV
      case outgF of 
        [] -> return (newSeenV,seenE)
        edgs -> foldM (\(seenV',seenE') edg@(MkEdge v1 v2) -> traverseGraph v2 (v1:seenV') (edg:seenE')) (newSeenV, seenE) edgs
    

runDepM :: DepM a b -> Either Error b
runDepM m = case runExcept (runStateT (getCheckM m) emptyGraph) of 
  Left err -> Left err
  Right (x,_) -> Right x
