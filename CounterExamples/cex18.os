module cex18

data X{MkX}
data Y{MkY}

-- multiple annotations for x
x :: X:CBV;
x :: Y:CBV;
x := MkX;
