module Common (
  firstJust,
  GetKind (..), 
  ShiftEvalOrder (..),
  ContainsKindvar (..),
  Typevar (..),
  Typename (..),
  Variable (..),
  FreeVariables (..),
  freshVar,
  FreeKindvars (..),
  freshKindvar,
  FreeTypevars (..),
  freshTypevar,
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

import Data.Set qualified as S

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

--------------------------------------------------------
-------------- Classes for Free Variables --------------
--------------------------------------------------------

class FreeVariables a where 
  freeVars :: a -> S.Set Variable 

instance FreeVariables a => FreeVariables [a] where 
  freeVars ls = S.unions (freeVars <$> ls)

freshVar :: FreeVariables a => a -> Variable
freshVar a = let frV = freeVars a in freshVarN 0 "x" Variable frV

class FreeKindvars a where 
  freeKindvars :: a -> S.Set Kindvar 

freshKindvar :: FreeKindvars a => a -> Kindvar 
freshKindvar a = let frV = freeKindvars a in freshVarN 0 "k" Kindvar frV

class FreeTypevars a where 
  freeTypevars :: a -> S.Set Typevar

freshTypevar :: FreeTypevars a => a -> Typevar 
freshTypevar a = let frV = freeTypevars a in freshVarN 0 "X" Typevar frV

freshVarN :: Eq a => Int -> String -> (String -> a) -> S.Set a -> a 
freshVarN n prefix ctor vars = let newV = ctor (prefix <> show n) in if newV `elem` vars then freshVarN (n+1) prefix ctor vars else newV

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

instance FreeKindvars Kind where
  freeKindvars (MkKind _) = S.empty 
  freeKindvars (MkKindVar v) = S.singleton v

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

