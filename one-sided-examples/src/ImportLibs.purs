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
Tuple "streamSrc" streamSrc,Tuple "natSrc" natSrc,Tuple "boolSrc" boolSrc,Tuple "lpairSrc" lpairSrc,Tuple "listSrc" listSrc,Tuple "pairSrc" pairSrc,Tuple "funSrc" funSrc,Tuple "unitSrc" unitSrc
]
