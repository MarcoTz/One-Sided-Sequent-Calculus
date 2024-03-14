module Fun

import Unit;
data Fun(a:+,b:-):- 
{ Ap(a,b)
}

id :: forall X. Fun(X,X) : +;
id := case { Ap(x,a) => <x | + | a> };

id2 :: Forall X. Fun(X,X) : +;
id2 := id;
