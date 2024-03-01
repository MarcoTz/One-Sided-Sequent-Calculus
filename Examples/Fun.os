module Fun
data Fun(a:+,b:-):- 
{
  Ap(a,b)
}

id :: forall X. Fun(X,X)
id := case { Ap(x,a) => <x | + | a> };

nothing := mu x.<case { Ap(y,z) => Done } | + | x>;

