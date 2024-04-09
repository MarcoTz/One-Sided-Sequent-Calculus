module Unit

data Unit { MkUnit }

posU :: {Unit};
posU := {MkUnit};

negU :: {Unit};
negU := {x}.<x|CBV|case {MkUnit => Done }>;

cutPos :: Unit; 
cutPos := Mu y. (MkUnit >> Unit >> Mu x. Done);

cutNeg :: Unit;
cutNeg := Mu y. (MkUnit << Unit << Mu x. Done); 
