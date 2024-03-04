module Nat 

data Nat:+ { 
  Z,
  S(Nat)
}

nat1 :: Nat 
nat1 := S(S(Z));

nat2 :: Nat 
nat2 := case { Z => Done, S(n) => Done};
