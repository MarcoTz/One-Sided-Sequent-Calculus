module Fun
data Fun(a:+,b:-):- 
{
  Ap(a,b)
}

data Bool:+ { True, False }

id :: Fun(Bool,Bool)
id := case { Ap(x,a) => <x | + | a> };

coId :: co (Fun(co Bool,co Bool))
coId := id;

nothing :: Fun(Bool,Bool)
nothing := mu x.<case { Ap(y,z) => Done } | + | x>;

