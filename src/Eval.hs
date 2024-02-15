module Eval where 

import Syntax 
isValue :: Pol -> Term -> Bool
isValue Pos (Var _) = True 
isValue Pos (Xtor _ args) = all (isValue Pos) args
isValue Pos (XCase _) = True
isValue Pos (Shift _) = True
isValue Pos _ = False 
isValue Neg _ = True

evalOnce :: Command -> Command
evalOnce cmd = cmd
