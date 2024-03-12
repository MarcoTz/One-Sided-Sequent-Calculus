module List

import Fun;

-- Lists
data List(a:+) : + {
  Cons(a,List(a)),
  Nil
}


tail :: forall X. Fun(List(X),List(X)) : +;
tail := case { Ap(ls,a) => 
  < case { 
    Nil         => <Nil | + | a>,
    Cons(hd,rs) => <rs  | + | a>
  } | - | ls> 
};

--recursive definitions not implemented yet
--len :: forall X. Fun(List(X),Nat):+;
--len := case { Ap(ls,a) => 
--  < case {
--    Nil => <Z|+|a>,
--    Cons(l1,lrs) => 
--      <S ( mu b. <len | + | Ap(lrs,b)> ) | + | a>
--  } | - | ls>
--};
