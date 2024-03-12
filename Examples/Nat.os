module Nat 

import Fun;

data Nat:+ { 
  Z,
  S(Nat)
}


succ :: Fun(Nat,Nat) : +;
succ := case { Ap(n,a) => <S(n) | + | a> };

-- recursive 
--pred :: Fun(Nat,Nat) : + ;
--pred := case { Ap(n,a) => 
--  <  case {
--    Z => <Z|+|a>,
--    S(m) => <mu b. <pred | + | Ap(m,b)> |+|a>
--  } | - | n> 
--};
