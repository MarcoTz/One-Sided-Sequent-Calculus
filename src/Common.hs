module Common where 

newtype XtorName = MkXtorName String
  deriving (Eq)
newtype TypeVar  = MkTypeVar String
  deriving (Eq,Ord)
newtype TypeName = MkTypeName String 
  deriving (Eq,Ord)
newtype Variable = MkVar String
  deriving (Eq,Ord)
newtype KindVar  = MkKVar String
  deriving (Eq,Ord)
data PolVar = MkPolVar !TypeVar !Pol
  deriving (Eq,Ord)


data Pol = Pos | Neg 
  deriving (Eq,Ord)
flipPol :: Pol -> Pol 
flipPol Pos = Neg 
flipPol Neg = Pos 

multPol :: Pol -> Pol -> Pol 
multPol Pos Pos = Pos 
multPol Pos Neg = Neg 
multPol Neg Pos = Neg 
multPol Neg Neg = Pos


data Kind = MkKind !Pol | MkKindVar !KindVar 
  deriving (Eq)

class GetKind a where 
  getKind :: a -> Pol

instance GetKind PolVar where 
  getKind (MkPolVar _ pol) = pol
