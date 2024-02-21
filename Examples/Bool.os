module Bool

--Booleans
data Bool : + { 
  True,
  False
}

var ifthenelse :=  Mu x. < x | + | case { True => Done, False => Done } >;
