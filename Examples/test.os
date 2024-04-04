module test
data Pair(a:+,b:+):+{
  Tup(a,b)
}
data Nat :+ {
  Z,
  S(Nat)
}
data Fun(a:+,b:-):- { 
  Ap(a,b)
}

ExitSuccess :: Forall X. X:-;
ExitSuccess := mu x.Done;
      
swap :: forall X Y. Fun(Pair(X,Y),Pair(Y,X)):+;
swap := case { Ap(p,a) =>
  < case { 
    Tup(b,c) => < Tup(c,b) | Pair(Y,X):+ | + | a>
    } | + | p>
};
      
pair1 :: Pair(Nat,Nat):+;
pair1 := Tup(Z,S(Z));
      
main := < swap | + | Ap(pair1,ExitSuccess)>;d

