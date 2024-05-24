module CounterExamples (getCex,numCex) where  

import Data.Array (intercalate, (!!), length)
import Data.Maybe (Maybe(..))

allCex :: Array (Array String)
allCex = [cex01,cex02,cex03,cex04,cex05,cex06,cex07,cex08,cex09,cex10,cex11,cex12,cex13,cex14,cex15,cex16,cex17]

numCex :: Int 
numCex = length allCex 

getCex :: Int -> String 
getCex n = case allCex !! n of 
    Nothing -> "" 
    Just cex -> intercalate "\n" cex

-- same xtor name in different types
cex01 :: Array String
cex01 = ["module cex01", 
  "data A {",
  "  MkA",
  "}",
  "" ,
  "codata B {",
  "  MkA",
  "}"]

-- same name for different types
cex02 :: Array String
cex02 = ["module cex02", 
  "data A {",
  "  MkA",
  "}",
  "",
  "data A{",
  "  MkB",
  "}"]

-- wrong number of type arguments for unit
cex03 :: Array String 
cex03 = ["module cex03",
  "import Unit;",
  "",
  "x :: Unit(Unit);",
  "x := Mu y.Done;"]

-- mutual recursion is not allowed
cex04 :: Array String
cex04 = ["module cex04", 
  "import Unit;",
  "",
  "test :: Unit;",
  "test := test2;",
  "test2 := test;"]

-- cannot use two cases in a cut
cex05 :: Array String 
cex05 = ["module cex05",
  "import Unit;",
  "doubleCns :: Unit;",
  "doubleCns := case {MkUnit => Done};",
  "",
  "main := <doubleCns | CBV | doubleCns>;"]

-- pattern without all xtors present
cex06 :: Array String
cex06 = ["module cex06",
  "data TwoCons{",
  "  MkOne,",
  "  MkTwo",
  "}",
  "badPat :: TwoCons;",
  "badPat := case {MkOne => Done};"]

-- type variable x appears free in annotation
cex07 :: Array String
cex07 = ["module cex07",
  "x :: X;",
  "x := Mu y.Done;"]

-- type variable a is not defined in type definition
cex08 :: Array String
cex08 = ["module cex08",
  "data X{",
  "  MkX(a)",
  "}"]

-- general type forall is not allowed in type definition
cex09 :: Array String
cex09 = ["module cex09",
  "data X{",
  "  MkX(Forall X.X)",
  "}"]

cex10 :: Array String
cex10 = ["module cex10",
  "x :: Unit;",
  "x := MkUnit;"
]
-- shift term cannot have co type, needs shifted type
cex11 :: Array String
cex11 = ["module cex11",
  "import Unit;",
  "x :: co(Unit);",
  "x := {MkUnit:CBV};"]

-- type annotation does not match 
cex12  :: Array String
cex12  = ["module cex12",
  "data X {MkX}",
  "data Y{MkY}",
  "",
  "x :: X;",
  "x := MkY;"]

-- type annotation is too general
cex13 :: Array String
cex13  = ["module cex13",
  "import Unit;",
  "x :: forall X.X;",
  "x := MkUnit;"]

-- wrong arity for mkunit
cex14 :: Array String
cex14 = ["module cex14",
  "import Unit;",
  "x :: Unit;",
  "x := MkUnit(MkUnit);"]

-- type annotation but missing definition
cex15 :: Array String
cex15 = ["module cex15",
  "import Unit;",
  "x :: Unit;"]

-- wrong type annotation
cex16 :: Array String
cex16 = ["module cex16",
  "data X{MkX}",
  "data Y{MkY}",
  "x :: X;",
  "x :: Y;",
  "x := MkX;"]

-- duplicate definitions of main
cex17 :: Array String 
cex17 = ["module cex17",
  "main := Done;",
  "main := Done;"]
