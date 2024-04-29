module Bool

import Fun

data Bool{ 
  True,
  False
}

not :: Fun(Bool,Bool)
not := case { Ap(b,a) => 
  <case {
    True  => <False | CBV | a>,
    False => <True  | CBV | a>
  } | CBV | b>
}

and :: Fun(Bool,Fun(Bool,Bool))
and := case { Ap(b1,a) =>
  < case { Ap(b2,b) => 
    < case {
      True => <b2|CBV|b>,
      False => <False|CBV|b>
    } | CBV | b1 > 
  } | CBV | a>
}
 
or :: Fun(Bool,Fun(Bool,Bool))
or := case { Ap(b1,a) => 
  < case { Ap(b2, b) => 
    < case { 
      True  => <True|CBV|b>,
      False => <b2  |CBV|b> 
    } | CBV | b1> 
  } | CBV | a> 
}

ifthenelse :: forall X. Bool -> X -> X 
ifthenelse := case { Ap(b1,a) => 
  < case { Ap(t1,b) => 
    < case { Ap(t2,c) => 
      < case { 
        True => <t1 | CBV | b>,
        False => <t2 | CBV | c> 
        } | CBV | b> 
    } | CBV | t2>
  } | CBV | t1>
}

printCons :: Forall X. X
printCons := mu x.Print x

andEx :: Bool
andEx := True && False 

orEx :: Bool
orEx := True || True 

notEx :: Bool
notEx := !True 

ifTrue :: Bool
ifTrue := if True then True else False

main := case True of { True => Done, False => error ""} 
