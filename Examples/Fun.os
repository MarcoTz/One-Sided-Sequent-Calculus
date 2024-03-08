module Fun

data Fun(a:+,b:-):- 
{
  Ap(a,b)
}

define Lam(x,y) := case { Ap(x,a) => <y | + | a> };
define App(x,y) := Mu a. <x | + | Ap(y,a)>;
