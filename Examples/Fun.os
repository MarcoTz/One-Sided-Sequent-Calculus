module Fun

import Unit;
codata Fun(a:+,b:-){ 
  Ap(a,b)
}

id :: forall X. Fun(X,X) : CBV;
id := case { Ap(x,a) => <x | CBV | a> };

id2 :: Forall X. Fun(X,X) : CBV;
id2 := id;
