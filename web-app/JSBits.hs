module JSBits where

import Foreign.C.String

setWithString :: (CString -> IO ()) -> String -> IO () 
setWithString fun str = withCString str fun

-- id evalSucc
foreign import javascript "((arr,offset) => document.getElementById('evalSucc').innerHTML = h$decodeUtf8z(arr,offset))"
  setEvalSucc :: CString -> IO ()

foreign import javascript "((x,y) => document.getElementById('successMessage').style.visibility='visible')"
  showEvalSucc :: CString -> IO()

foreign import javascript "((x,y) => document.getElementById('successMessage').style.visibility='hidden')"
  hideEvalSucc :: CString -> IO()

setSuccess :: String -> IO () 
setSuccess str = do 
  setWithString hideEvalError "x"
  setWithString showEvalSucc "x"
  setWithString setEvalSucc str

-- id evalErr
foreign import javascript "((arr,offset) => document.getElementById('evalError').innerHTML = h$decodeUtf8z(arr,offset))"
  setEvalError :: CString -> IO ()

foreign import javascript "(x,y) => document.getElementById('errorMessage').style.visibility='visible'"
  showEvalError :: CString -> IO()

foreign import javascript "(x,y) => document.getElementById('errorMessage').style.visibility='hidden'"
  hideEvalError :: CString -> IO()

setError :: String -> IO () 
setError str = do 
  setWithString hideEvalSucc "x"
  setWithString showEvalError "x"
  setWithString setEvalError str

