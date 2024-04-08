module Bool

import Fun;

data Bool{ 
  True,
  False
}

not :: Fun(Bool,Bool) : CBV ;
not := case { Ap(b,a) => 
  <case {
    True  => <False | CBV | a>,
    False => <True  | CBV | a>
  } | CBV | b>
};

and :: Fun(Bool,Fun(Bool,Bool)) : CBV;
and := case { Ap(b1,a) =>
  < case { Ap(b2,b) => 
    < case {
      True => <b2|CBV|b>,
      False => <False|CBV|b>
    } | CBV | b1 > 
  } | CBV | a>
};
 
or :: Fun(Bool,Fun(Bool,Bool)) : CBV;
or := case { Ap(b1,a) => 
  < case { Ap(b2, b) => 
    < case { 
      True  => <True|CBV|b>,
      False => <b2  |CBV|b> 
    } | CBV | b1> 
  } | CBV | a> 
};

printCons :: Forall X. X:CBN;
printCons := mu x.Print x;

main := <or | CBV | Ap(True, mu x. <x| CBV |Ap(True,printCons)>)>;
