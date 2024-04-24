module ImportLibs ( libSources ) where

import Data.Tuple (Tuple(..))
foreign import streamSrc :: String

foreign import natSrc :: String

foreign import boolSrc :: String

foreign import lpairSrc :: String

foreign import listSrc :: String

foreign import pairSrc :: String

foreign import funSrc :: String

foreign import unitSrc :: String

libSources :: Array (Tuple String String)
libSources = [
Tuple "stream" streamSrc,Tuple "nat" natSrc,Tuple "bool" boolSrc,Tuple "lpair" lpairSrc,Tuple "list" listSrc,Tuple "pair" pairSrc,Tuple "fun" funSrc,Tuple "unit" unitSrc
]
