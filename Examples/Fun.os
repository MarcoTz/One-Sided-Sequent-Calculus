module Fun
data Fun(a:+,b:-):- 
{
  Ap(a,b)
}

var id := case { Ap(x,a) => <x | + | a> };
var nothing := mu x.<case { Ap(y,z) => Done } | + | x>;

