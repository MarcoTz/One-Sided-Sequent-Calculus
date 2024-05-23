module Bool

import Fun

data Bool{ 
  True,
  False
}

not :: Bool -> Bool 
not := \b. mu a.<b |CBV |  
  case {
    True  => <False | CBV | a>,
    False => <True  | CBV | a>
  }>

and :: Bool -> Bool -> Bool
and := \b1. \b2. mu b.< case {
      True => <b2|CBV|b>,
      False => <False|CBV|b>
    } | CBV | b1 > 
   
or :: Bool -> Bool -> Bool
or := \b1. \b2. mu b.
    < case { 
      True  => <True|CBV|b>,
      False => <b2  |CBV|b> 
    } | CBV | b1> 

ifthenelse :: forall X. Bool -> X -> X -> X
ifthenelse :=\b. \t1.\t2. mu a. <case { True => <t1|CBV|a>, False => <t2|CBV|a> } |CBV|b> 
