module cex15

import Unit;

-- MkUnit should not have an argument
x :: Unit :CBV;
x := MkUnit(MkUnit);
