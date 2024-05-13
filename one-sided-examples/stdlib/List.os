module List

import Fun
import Nat
import Unit

-- Lists
data List(a:+){
  Cons(a,List(a)),
  Nil
}

brackEx :: List(Unit)
brackEx := [MkUnit,MkUnit,MkUnit]

brackEx2 :: List(Unit)
brackEx2 := [MkUnit,MkUnit,MkUnit,MkUnit,MkUnit]

tail :: forall X. Fun(List(X),List(X))
tail := case { Ap(ls,a) => 
  < case { 
    Nil         => <Nil | CBV | a>,
    Cons(hd,rs) => <rs  | CBV | a>
  } | CBV | ls> 
}

head :: forall X. Fun(List(X),X)
head := case { Ap(ls,a) => 
  < case { 
    Nil         => error "cannot take head of empty list",
    Cons(hd,rs) => <hd  | CBV | a>
  } | CBV | ls> 
}


len :: forall X. Fun(List(X),Nat)
rec len := case { Ap(ls,a) => 
  < case {
    Nil => <Z|CBV|a>,
    Cons(l1,lrs) => 
     <len | CBV | Ap(lrs,mu x.<S(x)|CBV|a>)>
  } | CBV | ls>
}

printCons :: Forall X. X
printCons := mu x. Print x

main := <len | Fun(List(Nat),Nat):CBV | Ap(Cons(Z,Nil),printCons)>
