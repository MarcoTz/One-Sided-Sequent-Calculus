module Loc (
  Loc (..),
  SourcePosition (..),
  defaultLoc,
  HasLoc,
  getLoc,
  setLoc
) where 

data SourcePosition = MkSourcePos {srcLine :: !Int, srcCol :: !Int}
  deriving (Eq,Ord)

defaultPos :: SourcePosition
defaultPos = MkSourcePos 0 0

data Loc = MkLoc { locStart :: !SourcePosition, locEnd :: !SourcePosition}
  deriving (Eq,Ord)

defaultLoc :: Loc 
defaultLoc = MkLoc defaultPos defaultPos

class HasLoc a where 
  getLoc :: a -> Loc
  setLoc :: Loc -> a -> a
