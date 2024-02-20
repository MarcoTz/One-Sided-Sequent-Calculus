module dupDecl

-- The same declaration cannot be used twice
data A : + {
  MkA
}

data A : - { 
  MkB
}
