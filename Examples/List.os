module List

import Fun;
import Nat;
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


len :: forall X. Fun(List(X),Nat):+;
rec len := case { Ap(ls,a) => 
  < case {
    Nil => <Z|+|a>,
    Cons(l1,lrs) => 
      <S ( mu b. <len | + | Ap(lrs,b)> ) | + | a>
  } | - | ls>
};

main := <len | Fun(List(Nat),Nat):+ | + | Ap(Cons(Z,Nil),mu x.Done)>;
