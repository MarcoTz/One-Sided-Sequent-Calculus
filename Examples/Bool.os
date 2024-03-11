module Bool

import Fun;

data Bool : + { 
  True,
  False
}


ifthenelse :: Forall X. Fun(Bool,Fun(X,Fun(X,X))) : -;
ifthenelse(b:Bool:+,t1:X:+,t2:X:+) := mu a. <case { True => <t1|+|a>, False =>  <t2|-|b>} | - | b >;

bool1 :: Bool:+;
bool1 := True;

bool2 :: Bool:+;
bool2 := False;
