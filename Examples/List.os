module List

import Fun;
import Unit;

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

head :: forall X. Fun(List(X),X) : +;
head := case { Ap(ls,a) => 
  < case { 
    Nil         => error "cannot take head of empty list",
    Cons(hd,rs) => <hd  | + | a>
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

main := <case { Nil => Done, Cons(x,y) => Done } | List(Unit):- | + | Nil>;
