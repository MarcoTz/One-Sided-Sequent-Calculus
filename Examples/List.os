module List

-- Lists
data List(a:+) : + {
  Cons(a,List(a)),
  Nil
}

data Bool : + {
  True, False
}

exList := Cons(True,Cons(False,Nil));
