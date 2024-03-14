module Fun

data Fun(a:+,b:-):- 
{ Ap(a,b)
}

data Unit : + { 
  MkUnit
}

id :: forall X. Fun(X,X) : +;
id := case { Ap(x,a) => <x | + | a> };

id2 :: Forall X. Fun(X,X) : +;
id2 := id;
