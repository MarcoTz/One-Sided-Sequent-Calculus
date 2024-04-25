module StandardLib (libMap) where 

import Common (Modulename(..))
import ImportLibs (libSources)

import Prelude ((<$>),(<>),(>>>))
import Data.Bifunctor (lmap)
import Data.Tuple (Tuple)
import Data.String.Common (toUpper)
import Data.String (uncons,singleton)
import Data.Maybe (Maybe(..))
import Data.Map (Map,fromFoldable)


libMap :: Map Modulename String 
libMap = do
  let libSourcesCap :: Array (Tuple Modulename String) 
      libSourcesCap = (lmap toModule) <$> libSources
  fromFoldable libSourcesCap
  where 
    toModule :: String -> Modulename
    toModule str = Modulename (fstUpper str)

    fstUpper :: String -> String 
    fstUpper str = 
      case uncons str of 
        Nothing -> "" 
        Just {head:hd,tail:tl} -> (singleton >>> toUpper) hd <> tl
        
