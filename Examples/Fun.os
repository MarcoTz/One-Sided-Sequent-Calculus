module Fun
data Fun(a:+,b:-):- 
{
  Ap(a,b)
}

var nothing := mu x.<case { Ap(y,z) => Done } | + | x>;
var id := Mu x. < case { Ap(y,b) => <y|+|b> } | + | x> ;

