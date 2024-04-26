

export const streamSrc = `
module Stream

import Bool

codata Stream(a:-){
  Head(a),
  Tail(Stream(a))
}

constTrue :: Stream(Bool)
rec constTrue := mu a. 
  <case { Head(b) => <True | CBV |b>, Tail(str) => < constTrue | CBV | str >  } | CBV | a>

`;

export const natSrc = `
module Nat 

import Fun

data Nat{ 
  Z,
  S(Nat)
}


succ :: Fun(Nat,Nat)
succ := case { Ap(n,a) => <S(n) | CBV | a> }

pred :: Fun(Nat,Nat)
rec pred := case { Ap(n,a) => 
  <  case {
    Z => <Z|CBV|a>,
    S(m) => <mu b. <pred | CBV | Ap(m,b)> |CBV|a>
  } | CBV | n> 
}

main := <pred | CBV | Ap(S(S(S(Z))),mu x.Print x)>
`;

export const boolSrc = `
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

ifthenelse :: forall X. Bool -> X -> X 
ifthenelse := case { Ap(b1,a) => 
  < case { Ap(t1,b) => 
    < case { Ap(t2,c) => 
      < case { 
        True => <t1 | CBV | b>,
        False => <t2 | CBV | c> 
        } | CBV | b> 
    } | CBV | t2>
  } | CBV | t1>
}

printCons :: Forall X. X
printCons := mu x.Print x

andEx :: Bool
andEx := True && False 

orEx :: Bool
orEx := True || True 

notEx :: Bool
notEx := !True 

ifTrue :: Bool
ifTrue := if True then True else False

main := <or | CBV | Ap(True, mu x. <x| CBV |Ap(True,printCons)>)>
`;

export const lpairSrc = `
module LPair 

codata LPair(a:-,b:-){ 
  fst(a),
  snd(b)
}
`;

export const listSrc = `
module List

import Fun
import Nat
import Unit

-- Lists
data List(a:+){
  Cons(a,List(a)),
  Nil
}

brackEx :: List(Uhit)
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
`;

export const pairSrc = `
module Pair

import Fun 
import Unit

data Pair(a:+,b:+) {
  Tup(a,b)
}

diag :: forall X. Fun(X,Pair(X,X))
diag := case { Ap(x,a) => <Tup(x,x) | CBV | a> }

pairSugar :: Pair(Unit,Unit)
pairSugar := (MkUnit,MkUnit,MkUnit,MkUnit)

pairSugar2 :: Pair(Unit,Unit)
pairSugar2 := (MkUnit,MkUnit,MkUnit)
`;

export const funSrc = `
module Fun

import Unit

codata Fun(a:+,b:-){ 
  Ap(a,b)
}

id :: forall X. Fun(X,X)
id := case { Ap(x,a) => <x | CBV | a> }

id2 :: Forall X. Fun(X,X)
id2 := \\x. x

main := <id2 [MkUnit] | CBV | mu x.Print x>
`;

export const unitSrc = `
module Unit

data Unit { MkUnit }

cbvU :: {Unit}
cbvU := {MkUnit:CBV}

cbnU :: {Unit}
cbnU := {MkUnit:CBN} 

cutCBV :: Unit 
cutCBV := Mu y. (MkUnit >> Unit >> Mu x. Done)

cutCBN :: Unit
cutCBN := Mu y. (MkUnit << Unit << Mu x. Done) 

sequence :: Unit 
sequence := MkUnit;MkUnit

main := case MkUnit of { MkUnit => Done}
`;
