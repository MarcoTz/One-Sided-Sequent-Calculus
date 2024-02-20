module List

data List(a:+) : + {
  Cons(a,List(a)),
  Nil
}
