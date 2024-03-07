module List

-- Lists
data List(a:+) : + {
  Cons(a,List(a)),
  Nil
}

data Bool : + {
  True, False
}

exList :: List(Bool)
exList := Cons(True,Cons(False,Nil));

head :: Bool 
head := mu x. <Cons(True,Cons(True,Nil)) | List(Bool) | + | case { Nil => < Nil | + | x >, Cons(a,b) => <a | + | x> } >;
