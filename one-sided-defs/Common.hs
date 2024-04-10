module Common (
  firstJust,
  GetKind (..), 
  ShiftEvalOrder (..),
  ContainsKindvar (..),
  Typevar (..),
  Typename (..),
  Variable (..),
  Xtorname (..),
  Variance (..),
  PrdCns (..),
  EvaluationOrder (..),
  defaultEo,
  DeclTy (..),
  VariantVar (..),
  varianceEvalOrder,
  Modulename (..),
  Kindvar (..),
  Kind (..)
) where 

----------------------
-- Helper Functions --
----------------------

firstJust :: [Maybe a] -> Maybe a 
firstJust [] = Nothing
firstJust (Nothing:as) = firstJust as 
firstJust (Just a: _) = Just a
-----------
-- Names --
-----------
newtype Modulename = Modulename {unModulename :: String}
  deriving (Eq,Ord)
newtype Xtorname = Xtorname {unXtorname :: String}
  deriving (Eq,Ord)
newtype Typename = Typename {unTypename :: String}
  deriving (Eq,Ord)

---------------
-- Variables --
---------------
newtype Typevar  = Typevar {unTypevar :: String}
  deriving (Eq,Ord)
newtype Variable = Variable {unVariable :: String} 
  deriving (Eq,Ord)
data PrdCns = Prd | Cns 
  deriving (Eq,Ord)
newtype Kindvar  = Kindvar {unKindvar :: String} 
  deriving (Eq,Ord)

--------------
-- Variance --
--------------
data Variance = Covariant | Contravariant
  deriving (Eq,Ord)
data VariantVar = VariantVar { variantVar :: !Typevar, variantVariance :: !Variance}
  deriving (Eq,Ord)

-----------
-- Kinds --
-----------
data EvaluationOrder = CBV | CBN
  deriving (Eq,Ord)
data Kind = MkKind !EvaluationOrder | MkKindVar !Kindvar 
  deriving (Eq,Ord)

class ShiftEvalOrder a where
 shiftEvalOrder :: a -> a  

instance ShiftEvalOrder EvaluationOrder where 
  shiftEvalOrder CBV = CBN
  shiftEvalOrder CBN = CBV

instance ShiftEvalOrder Kind where 
  shiftEvalOrder (MkKind eo) = MkKind $ shiftEvalOrder eo
  shiftEvalOrder v@MkKindVar{} = v

class ContainsKindvar a where 
  containsKindvar :: a -> Bool

instance ContainsKindvar Kind where 
  containsKindvar (MkKind _) = False
  containsKindvar (MkKindVar _) = True

class GetKind a where 
  getKind :: a -> Kind

varianceEvalOrder :: Variance -> EvaluationOrder -> EvaluationOrder 
varianceEvalOrder Covariant = id
varianceEvalOrder Contravariant = shiftEvalOrder

-----------------
-- Data/Codata --
-----------------
data DeclTy = Data | Codata
  deriving (Eq,Ord)

defaultEo :: DeclTy -> EvaluationOrder 
defaultEo Data = CBV
defaultEo Codata = CBN

