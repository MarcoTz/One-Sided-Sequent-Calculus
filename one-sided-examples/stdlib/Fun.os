module Fun

import Unit;

codata Fun(a:+,b:-){ 
  Ap(a,b)
}

id :: forall X. X -> X;
id := \x.x;
