module Fun

import Unit

codata Fun(a:+,b:-){ 
  Ap(a,b)
}

id :: forall X. Fun(X,X)
id := case { Ap(x,a) => <x | CBV | a> }

id2 :: Forall X. Fun(X,X)
id2 := \x. x

main := <id2 [MkUnit] | CBV | mu x.Print x>
