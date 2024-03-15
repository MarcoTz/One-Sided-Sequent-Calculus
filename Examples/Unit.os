module Unit

data Unit : + { MkUnit }

posU :: {Unit} : +;
posU := {MkUnit};

negU :: {Unit} : -;
negU := {x}.<x|+|case {MkUnit => Done }>;
