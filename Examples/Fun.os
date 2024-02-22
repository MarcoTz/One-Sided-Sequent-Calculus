module Fun
data Fun(a:+,b:-):- 
{
  Ap(a,b)
}

var nothing := mu x.<case { Ap(x,y) => Done } | + | x>;
var id := Mu x. mu y.<case { Ap(a,b) => <a|+|b> } | + | Ap(x,y)>;
