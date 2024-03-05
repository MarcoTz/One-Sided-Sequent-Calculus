module Fun
data Fun(a:+,b:-):- 
{
  Ap(a,b)
}

data Bool:+ { True, False }

id :: Fun(Bool,co Bool)
id := case { Ap(x,a) => <x | + | a> };

nothing :: Fun(Bool,co Bool)
nothing := mu x.<case { Ap(y,z) => Done } | + | x>;

