module Loc (
  Loc (..),
  SourcePosition (..),
  defaultLoc,
  class HasLoc,
  getLoc,
  setLoc,
  showLocInSource
) where 

import Prelude (class Eq, class Ord, class Show, show, (<>))
import Data.List (List(..), take, drop)
import Data.String as S
import Data.Array (toUnfoldable)

data SourcePosition = SourcePosition {srcLine :: Int, srcCol :: Int}
derive instance eqSourcePosition :: Eq SourcePosition
derive instance ordSOurcePosition :: Ord SourcePosition
instance Show SourcePosition where 
  show (SourcePosition {srcLine:line,srcCol:col}) = "Line : " <> show line <> ", Column: " <> show col
getLine :: SourcePosition -> Int 
getLine (SourcePosition {srcLine:line,srcCol:_}) = line
getCol :: SourcePosition -> Int 
getCol (SourcePosition {srcLine:_,srcCol:col}) = col

defaultPos :: SourcePosition
defaultPos = SourcePosition{srcLine:0, srcCol:0}


data Loc = Loc { locStart :: SourcePosition, locEnd :: SourcePosition}
derive instance eqLoc :: Eq Loc 
derive instance ordLoc :: Ord Loc
instance Show Loc where 
  show (Loc {locStart:startPos,locEnd:endPos}) = "Between " <> show startPos <> " and " <> show endPos
defaultLoc :: Loc 
defaultLoc = Loc{locStart:defaultPos,locEnd:defaultPos}


showLocInSource :: String -> Loc -> String
showLocInSource src (Loc loc) = do
  let srcLines = toUnfoldable (S.split (S.Pattern "\n") src)
  let posLines = betweenList (getLine loc.locStart) (getCol loc.locEnd) srcLines 
  case posLines of 
    Nil -> ""
    (Cons l1 _) -> betweenString (getCol loc.locStart) (getCol loc.locEnd) l1
  where 

    betweenList :: forall a. Int -> Int -> List a -> List a 
    betweenList start end ls = take end (drop start ls) 


    betweenString :: Int -> Int -> String -> String
    betweenString start end str = S.take end (S.drop start str)

class HasLoc a where 
  getLoc :: a -> Loc
  setLoc :: Loc -> a -> a
