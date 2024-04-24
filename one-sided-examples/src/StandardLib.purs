module StandardLib (libMap) where 

import ImportLibs (libSources)
import Data.Map (Map,fromFoldable)

libMap :: Map String String 
libMap = fromFoldable libSources
