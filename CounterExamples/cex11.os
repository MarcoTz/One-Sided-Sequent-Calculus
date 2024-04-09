module cex11

data X{
  MkX
}

data Y{
  MkY
}

-- type annotation for x is present, but type within cut is unclear
x :: X;
x := case { MkX => < MkY| CBV | case { MkY => Done }>}; 
