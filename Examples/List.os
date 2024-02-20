module List

-- Lists
data List(a:+) : + {
  Cons(a,List(a)),
  Nil
}
