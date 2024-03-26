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

instance Show SourcePosition where 
  show (MkSourcePos line col) = "Line : " <> show line <> ", Column: " <> show col

defaultPos :: SourcePosition
defaultPos = MkSourcePos 0 0

data Loc = MkLoc { locStart :: !SourcePosition, locEnd :: !SourcePosition}
  deriving (Eq,Ord)

instance Show Loc where 
  show (MkLoc startPos endPos) = "Between " <> show startPos <> " and " <> show endPos
defaultLoc :: Loc 
defaultLoc = MkLoc defaultPos defaultPos

class HasLoc a where 
  getLoc :: a -> Loc
  setLoc :: Loc -> a -> a
