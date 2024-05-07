module Pair

import Fun 
import Unit

data Pair(a:+,b:+) {
  Tup(a,b)
}

diag :: forall X. Fun(X,Pair(X,X))
diag := case { Ap(x,a) => <Tup(x,x) | CBV | a> }

pairSugar :: Pair(Unit,Pair(Unit,Pair(Unit,Unit)))
pairSugar := (MkUnit,MkUnit,MkUnit,MkUnit)

pairSugar2 :: Pair(Unit,Pair(Unit,Unit))
pairSugar2 := (MkUnit,MkUnit,MkUnit)
