module Fun
data Fun(a:+,b:-):- 
{
  Ap(a,b)
}

var nothing := mu x.<case { Ap(x,y) => Done } | + | x>;
var id := Mu x. <Mu a. < case { Ap(x,a) => <x|+|a> } | + | Ap(x,a)> | + | x> ;

