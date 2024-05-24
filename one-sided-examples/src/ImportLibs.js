

export const streamSrc = `
module Stream

import Bool;

codata Stream(a:-){
  Head(a),
  Tail(Stream(a))
}

constTrue :: Stream(Bool);
rec constTrue := mu a. 
  <case { Head(b) => <True | CBV |b>, Tail(str) => < constTrue | CBV | str >  } | CBV | a>;

`;

export const maybeSrc = `
module Maybe

import Fun;
import Bool;

data Maybe(a:+) { 
  Nothing,
  Just(a)
  }

maybe :: forall a b. b -> (a -> b) -> Maybe(a) -> b;
maybe := \\b. \\f. \\a. mu c. < a | CBV | case { Nothing => <b|CBV|c>, Just(a) => <f a|CBV|c> }>;

isJust :: forall a. Maybe(a) -> Bool;
isJust := \\a. mu c. < a | CBV | case { Nothing => <False|CBV|c>, Just(a) => <True|CBV|c> }>;

isNothing :: forall a. Maybe(a) -> Bool;
isNothing := \\a. mu c. <a | CBV | case { Nothing => <True|CBV|c>, Just(a) => <False|CBV|c>}>;

fromJust :: forall a. Maybe(a) -> a;
fromJust := \\a. mu c. <a|CBV|case {Nothing => error "expected Just", Just(x) => <x|CBV|c>}>;

fromMaybe :: forall a. a -> Maybe(a) -> a;
fromMaybe := \\a. \\ma. mu c. <ma|CBV|case {Nothing => <a|CBV|c>, Just(a1)=><a1|CBV|c>}>;
`;

export const natSrc = `
module Nat 

import Fun;

data Nat{ 
  Z,
  S(Nat)
}


succ :: Nat -> Nat;
succ := \\n. S(n);

pred :: Nat -> Nat;
pred := \\n. mu a.
  <  case {
    Z    => error "Cannot take predecessor of 0",
    S(m) => <m|CBV|a> 
  } | CBV | n>;
`;

export const boolSrc = `
module Bool

import Fun;

data Bool{ 
  True,
  False
}

not :: Bool -> Bool;
not := \\b. mu a.<b |CBV |  
  case {
    True  => <False | CBV | a>,
    False => <True  | CBV | a>
  }>;

and :: Bool -> Bool -> Bool;
and := \\b1. \\b2. mu b.< case {
      True => <b2|CBV|b>,
      False => <False|CBV|b>
    } | CBV | b1 >;
   
or :: Bool -> Bool -> Bool;
or := \\b1. \\b2. mu b.
    < case { 
      True  => <True|CBV|b>,
      False => <b2  |CBV|b> 
    } | CBV | b1>;

ifthenelse :: forall X. Bool -> X -> X -> X;
ifthenelse :=\\b. \\t1.\\t2. mu a. <case { True => <t1|CBV|a>, False => <t2|CBV|a> } |CBV|b>;
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

import Fun;
import Nat;
import Unit;
import Prelude;

data List(a:+){
  Cons(a,List(a)),
  Nil
}

tail :: forall X. List(X)->List(X);
tail := \\ls. mu a. 
  < case { 
    Nil         => error "Cannot take tail of empty list",
    Cons(hd,rs) => <rs  | CBV | a>
  } | CBV | ls>;

head :: forall X. List(X) -> X;
head := \\ls. mu a. 
  < case { 
    Nil         => error "cannot take head of empty list",
    Cons(hd,rs) => <hd  | CBV | a>
  } | CBV | ls>;


len :: forall X. List(X) -> Nat;
rec len := \\ls. mu a.  
  < case {
    Nil => <Z|CBV|a>,
    Cons(l1,lrs) => <len lrs | CBV| a>
  } | CBV | ls>;

-- fix this
--take :: forall X. Nat -> List(X) -> List(X);
--take := \\n.\\ls. mu a.<n | CBV | 
--  case { 
--    Z    => <Nil|CBV|a>,
--    S(m) => <ls | CBV | 
--      case { 
--        Nil        => error "Cannot take nonzero elements from empty list",
--        Cons(x,xs) => < take m | CBV | a> 
--      }>
--  }>;

main := <len Cons(Z,Nil) | CBV | printT>;
`;

export const preludeSrc = `
module Prelude

printT :: forall a. a;
printT := mu x. Print x;

exitSucc :: forall a. a;
exitSucc := mu x. Done;
`;

export const pairSrc = `
module Pair

import Fun;
import Unit;

data Pair(a:+,b:+) {
  Tup(a,b)
}

diag :: forall X. X -> Pair(X,X);
diag := \\x. (x,x);

uncurry :: forall X Y Z. (X -> Y -> Z) -> Pair(X,Y) -> Z;
uncurry := \\f. \\tp. mu a. <tp | CBV | case { Tup(x,y) => <f x y|CBV|a> }>;
`;

export const funSrc = `
module Fun

import Unit;

codata Fun(a:+,b:-){ 
  Ap(a,b)
}

id :: forall X. X -> X;
id := \\x.x;
`;

export const unitSrc = `
module Unit

data Unit { MkUnit }
`;
