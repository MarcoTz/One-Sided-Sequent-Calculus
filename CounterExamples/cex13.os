module cex13 

data X {MkX}
data Y{MkY}

-- mismatching annotation
x :: X :CBV;
x := MkY;
