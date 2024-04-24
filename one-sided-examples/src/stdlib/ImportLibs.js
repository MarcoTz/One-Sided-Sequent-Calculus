export const unitSrc = 
`module Unit

data Unit { MkUnit }

cbvU :: {Unit};
cbvU := {MkUnit:CBV};

cbnU :: {Unit};
cbnU := {MkUnit:CBN}; 

cutCBV :: Unit; 
cutCBV := Mu y. (MkUnit >> Unit >> Mu x. Done);

cutCBN :: Unit;
cutCBN := Mu y. (MkUnit << Unit << Mu x. Done); `
