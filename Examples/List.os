module List

-- Lists
data List(a:+) : + {
  Cons(a,List(a)),
  Nil
}

data Bool : + {
  True, False
}

var exList := Cons(True,Cons(False,Nil));
