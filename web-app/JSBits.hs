module JSBits where

import Foreign.C.String

setWithString :: (CString -> IO ()) -> String -> IO () 
setWithString fun str = withCString str fun

-- id resultStr
foreign import javascript "((arr,offset) => {console.log(h$decodeUtf8z(arr,offset)); document.getElementById('resultStr').innerHTML = h$decodeUtf8z(arr,offset)})"
  setResString :: CString -> IO ()
foreign import javascript "((arr,offset) => document.getElementById('resultStr').className = h$decodeUtf8z(arr,offset))"
  setResClass :: CString -> IO ()

setSuccess :: String -> IO () 
setSuccess str = do 
  setWithString setResClass "evalSucc"
  setWithString setResString str


setError :: String -> IO () 
setError str = do 
  setWithString setResClass "evalError"
  setWithString setResString str

