module Fun

data Fun(a:+,b:-):- 
{
  Ap(a,b)
}

data Unit : + { 
  MkUnit
}

Lam :: Fun(Unit,Unit);
Lam(x,y) := case { Ap(x,a) => <y | + | a> };
App :: Unit;
App(x,y) := Mu a. <x | + | Ap(y,a)>;
