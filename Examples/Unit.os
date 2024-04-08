module Unit

data Unit { MkUnit }

posU :: {Unit} : CBV;
posU := {MkUnit};

negU :: {Unit} : CBN;
negU := {x}.<x|CBV|case {MkUnit => Done }>;

cutPos :: Unit : CBV; 
cutPos := Mu y. (MkUnit >> Unit:CBV >> Mu x. Done);

cutNeg :: Unit : CBV;
cutNeg := Mu y. (MkUnit << Unit:CBV << Mu x. Done); 
