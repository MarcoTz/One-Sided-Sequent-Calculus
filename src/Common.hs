module Common where 

type XtorName = String
type TypeVar = String
type TypeName = String 


data Pol = Pos | Neg 
  deriving (Eq)
flipPol :: Pol -> Pol 
flipPol Pos = Neg 
flipPol Neg = Pos 

multPol :: Pol -> Pol -> Pol 
multPol Pos Pos = Pos 
multPol Pos Neg = Neg 
multPol Neg Pos = Neg 
multPol Neg Neg = Pos


type Variable = String

type KindVar = String
data Kind = MkKind !Pol | MkKindVar !KindVar 
  deriving (Eq)

class GetKind a where 
  getKind :: a -> Pol
