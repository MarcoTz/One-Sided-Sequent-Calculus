module Bool

import Fun;

--Booleans
data Bool : + { 
  True,
  False
}

ifthenelse :: Bool:+;
ifthenelse :=  Mu x. < x | + | case { True => Done, False => Done } >;

bool1 :: Bool:+;
bool1 := True;

bool2 :: Bool:+;
bool2 := False;
