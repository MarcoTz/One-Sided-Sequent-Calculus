module Loc (
  Loc (..),
  SourcePos (..),
  defaultLoc
) where 

data SourcePos = MkSourcePos !FilePath !Int !Int

defaultPos :: SourcePos 
defaultPos = MkSourcePos "" 0 0

data Loc = MkLoc !SourcePos !SourcePos

defaultLoc :: Loc 
defaultLoc = MkLoc defaultPos defaultPos
