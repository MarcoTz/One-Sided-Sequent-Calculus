module Common where 

newtype Modulename = MkModule String
  deriving (Eq,Ord)
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

class FlipPol a where 
  flipPol :: a -> a 

instance FlipPol Pol where
  flipPol Pos = Neg 
  flipPol Neg = Pos 

data Kind = MkKind !Pol | MkKindVar !KindVar 
  deriving (Eq)

class GetKind a where 
  getKind :: a -> Pol

instance GetKind PolVar where 
  getKind (MkPolVar _ pol) = pol
