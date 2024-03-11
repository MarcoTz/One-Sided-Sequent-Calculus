module Fun

data Fun(a:+,b:-):- 
{
  Ap(a,b)
}

data Unit : + { 
  MkUnit
}

Lam :: Fun(Unit,Unit) : +;
Lam(x:Unit : +,y:Unit : +) := case { Ap(x,a) => <y | + | a> };
App :: Unit : - ;
App(x:Fun(Unit,Unit):+,y:Unit:+) := Mu a. <x | + | Ap(y,a)>;
