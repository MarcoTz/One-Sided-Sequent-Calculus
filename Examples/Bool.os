module Bool

import Fun;

data Bool : + { 
  True,
  False
}

not :: Fun(Bool,Bool) : + ;
not := case { Ap(b,a) => 
  <case {
    True  => <False | + | a>,
    False => <True  | + | a>
  } | - | b>
};

and :: Fun(Bool,Fun(Bool,Bool)) : +;
and := case { Ap(b1,a) =>
  < case { Ap(b2,b) => 
    < case {
      True => <b2|+|b>,
      False => <False|+|b>
    } | - | b1 > 
  } | + | a>
};
 
or :: Fun(Bool,Fun(Bool,Bool)) : +;
or := case { Ap(b1,a) => 
  < case { Ap(b2, b) => 
    < case { 
      True  => <True|+|b>,
      False => <b2  |+|b> 
    } | - | b1> 
  } | + | a> 
};
