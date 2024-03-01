module Bool

--Booleans
data Bool : + { 
  True,
  False
}

ifthenelse :=  Mu x. < x | + | case { True => Done, False => Done } >;
