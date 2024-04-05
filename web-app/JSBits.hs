module JSBits where

import Foreign.C.String

setWithString :: (CString -> IO ()) -> String -> IO () 
setWithString fun str = withCString str fun

-- id resultStr
foreign import javascript "((arr,offset) => document.getElementById('resultStr').innerHTML = h$decodeUtf8z(arr,offset))"
  setResString :: CString -> IO ()
foreign import javascript "((arr,offset) => document.getElementById('resultStr').className = h$decodeUtf8z(arr,offset))"
  setResClass :: CString -> IO ()
-- id traceStr
foreign import javascript "((arr,offset) => document.getElementById('traceStr').innerHTML = h$decodeUtf8z(arr,offset))"
  setTraceString :: CString -> IO () 
-- id typesStr
foreign import javascript "((arr,offset) => document.getElementById('typesStr').innerHTML = h$decodeUtf8z(arr,offset))"
  setTypesString :: CString -> IO ()

setSuccess :: String -> String -> String -> IO () 
setSuccess res tr tys = do 
  setWithString setResClass "evalSucc"
  setWithString setResString res
  setWithString setTraceString tr
  setWithString setTypesString tys


setError :: String -> IO () 
setError str = do 
  setWithString setResClass "evalError"
  setWithString setResString str
  setWithString setTraceString ""
  setWithString setTypesString ""

