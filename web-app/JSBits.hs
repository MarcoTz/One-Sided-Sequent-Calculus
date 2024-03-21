module JSBits where

import Foreign.C.String

-- "id=evalResult"

foreign import javascript "((arr,offset) => document.getElementById('evalResult').innerHTML = h$decodeUtf8z(arr,offset))"
  setEvaluationResultInternal :: CString -> IO ()

setEvaluationResult :: String -> IO ()
setEvaluationResult s = withCString s setEvaluationResultInternal
