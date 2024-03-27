module Loc (
  Loc (..),
  SourcePosition (..),
  defaultLoc,
  HasLoc,
  getLoc,
  setLoc,
  showLocInSource
) where 


data SourcePosition = MkSourcePos {srcLine :: !Int, srcCol :: !Int}
  deriving (Eq,Ord)

instance Show SourcePosition where 
  show (MkSourcePos line col) = "Line : " <> show line <> ", Column: " <> show col

showLocInSource :: String -> Loc -> String
showLocInSource src loc = do
  let endPos = locEnd loc
  let untilLine = unt (srcLine endPos) (lines src)
  let untilColFun = unt (srcCol endPos) 
  let untilCol = updateLast untilLine untilColFun
  let startPos = locStart loc
  let fromLine = frm (srcLine startPos) untilCol
  let fromColFun = frm (srcCol startPos)
  let fromCol = updateFirst fromLine fromColFun
  concat fromCol
  where 
    unt :: Int -> [a] -> [a]
    unt _ [] = [] 
    unt i _ | i < 0 = []
    unt i (a:as) = a:unt (i-1) as

    updateLast :: [a] -> (a->a) -> [a]
    updateLast [] _ = [] 
    updateLast [a] fun = [fun a]
    updateLast (a:as) fun = a:updateLast as fun

    frm :: Int -> [a] ->[a]
    frm 1 ls = ls
    frm i _ | i<=0 = [] 
    frm _ [] = []
    frm i (_:as) = frm (i-1) as

    updateFirst :: [a] -> (a->a) -> [a]
    updateFirst [] _ = []
    updateFirst (a:as) fun = fun a : as 


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
