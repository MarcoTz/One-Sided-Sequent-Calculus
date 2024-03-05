module Fun
data Fun(a:+,b:-):- 
{
  Ap(a,b)
}

data Bool:+ { True, False }

id :: Fun(co Bool,Bool)
id := case { Ap(x,a) => <x | + | a> };

nothing :: Fun(co Bool,Bool)
nothing := mu x.<case { Ap(y,z) => Done } | + | x>;

