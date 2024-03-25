module Loc (
  Loc (..),
  SourcePos (..),
  defaultLoc,
  HasLoc,
  getLoc,
  setLoc
) where 

data SourcePos = MkSourcePos !FilePath !Int !Int
  deriving (Eq,Ord)

defaultPos :: SourcePos 
defaultPos = MkSourcePos "" 0 0

data Loc = MkLoc !SourcePos !SourcePos
  deriving (Eq,Ord)

defaultLoc :: Loc 
defaultLoc = MkLoc defaultPos defaultPos

class HasLoc a where 
  getLoc :: a -> Loc
  setLoc :: Loc -> a -> a
