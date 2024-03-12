module Bool

import Fun;

data Bool : + { 
  True,
  False
}


ifthenelse :: Forall X Y. Fun(Bool,Fun(X,Y)): +;
ifthenelse := 
  case { Ap(b,a) => 
    <case { 
      True => 
        < case { Ap(t1,c) => <t1| + | c> } | + | a >,
      False => 
        < case { Ap(t2,d) => <t2| + | d> } | + | a > 
      }
    | - | b >}; 

bool1 :: Bool:+;
bool1 := True;

bool2 :: Bool:+;
bool2 := False;
