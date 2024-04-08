module List

import Fun;
import Nat;
import Unit;

-- Lists
data List(a:+){
  Cons(a,List(a)),
  Nil
}

tail :: forall X. Fun(List(X),List(X)) : CBV;
tail := case { Ap(ls,a) => 
  < case { 
    Nil         => <Nil | CBV | a>,
    Cons(hd,rs) => <rs  | CBV | a>
  } | CBV | ls> 
};

head :: forall X. Fun(List(X),X) : CBV;
head := case { Ap(ls,a) => 
  < case { 
    Nil         => error "cannot take head of empty list",
    Cons(hd,rs) => <hd  | CBV | a>
  } | CBV | ls> 
};


len :: forall X. Fun(List(X),Nat):CBV;
rec len := case { Ap(ls,a) => 
  < case {
    Nil => <Z|CBV|a>,
    Cons(l1,lrs) => 
     <len | CBV | Ap(lrs,mu x.<S(x)|CBV|a>)>
  } | CBV | ls>
};

printCons :: Forall X. X:CBN;
printCons := mu x. Print x;

main := <len | Fun(List(Nat),Nat):CBV | CBV | Ap(Cons(Z,Nil),printCons)>;
