module Bool

--Booleans
data Bool : + { 
  True,
  False
}

val ifthenelse =  Mu x. < x | + | case { True => Done, False => Done } >;
