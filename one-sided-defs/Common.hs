module Common (
  FlipPol, 
  flipPol,
  GetKind, 
  getKind,
  Typevar (..),
  Pol (..),
  Typename (..),
  Variable (..),
  Xtorname (..),
  Modulename (..),
  Polvar (..),
  Kindvar (..),
  Kind (..)
) where 

newtype Modulename = Modulename {unModulename :: String}
  deriving (Eq,Ord)
newtype Xtorname = Xtorname {unXtorname :: String}
  deriving (Eq,Ord)
newtype Typevar  = Typevar {unTypevar :: String}
  deriving (Eq,Ord)
newtype Typename = Typename {unTypename :: String}
  deriving (Eq,Ord)
newtype Variable = Variable {unVariable :: String} 
  deriving (Eq,Ord)
newtype Kindvar  = Kindvar {unKindvar :: String} 
  deriving (Eq,Ord)
data Polvar = Polvar { polvarVar :: !Typevar, polvarPol :: !Pol}
  deriving (Eq,Ord)
data Pol = Pos | Neg 
  deriving (Eq,Ord)

class FlipPol a where 
  flipPol :: a -> a 

instance FlipPol Pol where
  flipPol Pos = Neg 
  flipPol Neg = Pos 

multPol :: Pol -> Pol -> Pol
multPol Pos Neg = Neg 
multPol Neg Pos = Neg 
multPol Pos Pos = Pos 
multPol Neg Neg = Pos

data Kind = MkKind !Pol | MkKindVar !Kindvar 
  deriving (Eq)

class GetKind a where 
  getKind :: a -> Pol

instance GetKind Polvar where 
  getKind (Polvar _ pol) = pol
