module Nat 

import Fun;

data Nat{ 
  Z,
  S(Nat)
}


succ :: Nat -> Nat;
succ := \n. S(n);

pred :: Nat -> Nat;
pred := \n. mu a.
  <  case {
    Z    => error "Cannot take predecessor of 0",
    S(m) => <m|CBV|a> 
  } | CBV | n>;
