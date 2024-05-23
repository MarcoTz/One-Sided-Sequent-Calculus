module List

import Fun
import Nat
import Unit
import Prelude

data List(a:+){
  Cons(a,List(a)),
  Nil
}

tail :: forall X. List(X)->List(X) 
tail := \ls. mu a. 
  < case { 
    Nil         => error "Cannot take tail of empty list",
    Cons(hd,rs) => <rs  | CBV | a>
  } | CBV | ls> 

head :: forall X. List(X) -> X 
head := \ls. mu a. 
  < case { 
    Nil         => error "cannot take head of empty list",
    Cons(hd,rs) => <hd  | CBV | a>
  } | CBV | ls>


len :: forall X. List(X) -> Nat 
rec len := \ls. mu a.  
  < case {
    Nil => <Z|CBV|a>,
    Cons(l1,lrs) => <len [lrs] | CBV| a>
  } | CBV | ls>

-- fix this
--take :: forall X. Nat -> List(X) -> List(X)
--take := \n.\ls. mu a.<n | CBV | 
--  case { 
--    Z    => <Nil|CBV|a>,
--    S(m) => <ls | CBV | 
--      case { 
--        Nil        => error "Cannot take nonzero elements from empty list",
--        Cons(x,xs) => < take [m] | CBV | a> 
--      }>
--  }>

main := <len [Cons(Z,Nil)] | CBV | printT>
