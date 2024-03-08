module List

import Bool; 

-- Lists
data List(a:+) : + {
  Cons(a,List(a)),
  Nil
}


exList :: List(Bool);
exList := Cons(True,Cons(False,Nil));

head :: Bool;
head := mu x:-. <Cons(True,Cons(True,Nil)) | List(Bool) | + | case { Nil => < True | + | x >, Cons(a,b) => <a | + | x> } >;
