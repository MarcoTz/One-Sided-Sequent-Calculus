module Loc (
  Loc (..),
  SourcePosition (..),
  defaultLoc,
  class HasLoc,
  getLoc,
  setLoc,
  showLocInSource
) where 

import Prelude (show, (<>), (+),(-))
import Data.List (List(..), take, drop)
import Data.String as S
import Data.Array (toUnfoldable)

type SourcePosition = {srcLine :: Int, srcCol :: Int}

defaultPos :: SourcePosition
defaultPos = {srcLine:0, srcCol:0}


type Loc = { locStart :: SourcePosition, locEnd :: SourcePosition}
defaultLoc :: Loc 
defaultLoc = {locStart:defaultPos,locEnd:defaultPos}


showLocInSource :: String -> Loc -> String
showLocInSource src loc = do
  let srcLines = toUnfoldable (S.split (S.Pattern "\n") src)
  let posLines = betweenList (loc.locStart.srcLine-1) loc.locEnd.srcCol srcLines 
  case posLines of 
    Nil -> ""
    (Cons l1 _) -> do
      let offset = 2
      let msg = betweenString (loc.locStart.srcCol - offset) (loc.locEnd.srcCol+offset) l1
      let startMsg = "line " <> show (loc.locStart.srcLine) <> ", " <> show loc.locStart.srcCol
      let endMsg = "line " <> show loc.locEnd.srcLine <> ", " <> show loc.locEnd.srcCol
      msg <> "\n" <> startMsg <> "-" <> endMsg
  where 

    betweenList :: forall a. Int -> Int -> List a -> List a 
    betweenList start end ls = take end (drop start ls) 

    betweenString :: Int -> Int -> String -> String
    betweenString start end str = S.take end (S.drop start str)

class HasLoc a where 
  getLoc :: a -> Loc
  setLoc :: Loc -> a -> a
