module Fun

import Unit;

codata Fun(a:+,b:-){ 
  Ap(a,b)
}

id :: forall X. X -> X;
id := \x.x;

fix := \f.\x. (f (x x)) (\x.(f (x x)));
