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

ifthenelse :: forall X. Bool -> X -> X -> X
ifthenelse :=\b. \t1.\t2. mu a. <case { True => <t1|CBV|a>, False => <t2|CBV|a> } |CBV|b> 

andEx :: Bool
andEx := True && False 

orEx :: Bool
orEx := True || True 

notEx :: Bool
notEx := !True 

ifTrue :: Bool
ifTrue := if True then True else False

main := case True of { True => Done, False => error ""} 
