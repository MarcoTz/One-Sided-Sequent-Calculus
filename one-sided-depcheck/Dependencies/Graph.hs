module Dependencies.Graph (
  Edge (..),
  Vertex,
  Graph (..),
  emptyGraph,
  getEdgesStartingAt,
  getEdgesEndingAt,
  addEdge,
  addVertex,
  getStartingVert,
  getVertex,
  getVertexLabel
) where

import Data.Set qualified as S
import Data.List (find) 

newtype Vertex a = MkVertex a
  deriving (Eq, Ord, Show)
data Edge   a = MkEdge   !(Vertex a) !(Vertex a)
  deriving (Eq, Show) 
data Graph  a = MkGraph {grVerts :: !(S.Set (Vertex a)), grEdges :: ![Edge a]}
  deriving (Show)

emptyGraph :: Graph a 
emptyGraph = MkGraph S.empty []

addVertex :: Eq a => Ord a => a -> Graph a -> (Vertex a, Graph a)
addVertex v gr@(MkGraph verts edges) = 
  case getVertex v gr of 
    Just vert -> (vert,gr)
    Nothing ->  
      let newVert = MkVertex v in
      (newVert, MkGraph (S.insert newVert verts) edges)

getVertex :: Eq a => a -> Graph a -> Maybe (Vertex a)
getVertex lb (MkGraph verts _) = find (\(MkVertex lb') -> lb == lb') verts 

getVertexLabel :: Vertex a -> a 
getVertexLabel (MkVertex l) = l

addEdge :: Eq a => (Vertex a,Vertex a) -> Graph a -> Graph a
addEdge (a1,a2) gr@(MkGraph verts edges) = 
  if MkEdge a1 a2 `elem` edges then gr else
  MkGraph verts  (MkEdge a1 a2 : edges) 

getStartingVert :: Edge a -> Vertex a
getStartingVert (MkEdge v _) = v

getEndingVert :: Edge a -> Vertex a 
getEndingVert (MkEdge _ v) = v

getEdgesStartingAt :: Eq a => Vertex a -> Graph a -> [Edge a]
getEdgesStartingAt vert (MkGraph _ edges) = filter (\e -> getStartingVert e==vert) edges

getEdgesEndingAt :: Eq a => Vertex a -> Graph a -> [Edge a]
getEdgesEndingAt vert (MkGraph _ edges) = filter (\e -> getEndingVert e==vert) edges
