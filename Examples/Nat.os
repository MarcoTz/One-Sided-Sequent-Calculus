module Nat 

import Fun;

data Nat{ 
  Z,
  S(Nat)
}


succ :: Fun(Nat,Nat) : CBV;
succ := case { Ap(n,a) => <S(n) | CBV | a> };

pred :: Fun(Nat,Nat) : CBV ;
rec pred := case { Ap(n,a) => 
  <  case {
    Z => <Z|CBV|a>,
    S(m) => <mu b. <pred | CBV | Ap(m,b)> |CBV|a>
  } | CBV | n> 
};

main := <pred | CBV | Ap(S(S(S(Z))),mu x.Print x)>;
