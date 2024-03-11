module Fun

data Fun(a:+,b:-):- 
{ Ap(a,b)
}

data Unit : + { 
  MkUnit
}

Lam :: Forall X. Fun(X,X) : +;
Lam(x:X : +,y:X : +) := case { Ap(x,a) => <y | + | a> };
App :: Unit : - ;
App(x:Fun(,Unit):+,y:Unit:+) := Mu a. <x | + | Ap(y,a)>;

id :: forall X. Fun(X,X) : +;
id := case { Ap(x,a) => <x | + | a> };
