module Common (
  firstJust,
  class GetKind, 
  getKind,
  class ShiftEvalOrder,
  shiftEvalOrder,
  class ContainsKindvar,
  containsKindvar,
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
  Kind (..),
  freshVarN
) where 

import Prelude (class Eq, class Ord, class Show, (<>), show, (+), ($), identity)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Set (Set, member) 

----------------------
-- Helper Functions --
----------------------

firstJust :: forall a.List (Maybe a) -> Maybe a 
firstJust Nil = Nothing
firstJust (Cons Nothing as) = firstJust as 
firstJust (Cons (Just a) _) = Just a

-----------
-- Names --
-----------
data Modulename = Modulename String 
derive instance eqModulename :: Eq Modulename
derive instance ordModulename :: Ord Modulename
instance Show Modulename where 
  show (Modulename nm) = nm

data Xtorname = Xtorname String 
derive instance eqXtorname :: Eq Xtorname
derive instance ordXtorname :: Ord Xtorname
instance Show Xtorname where 
  show (Xtorname nm) = nm 

data Typename = Typename String
derive instance eqTypename :: Eq Typename
derive instance ordTypename :: Ord Typename
instance Show Typename where 
  show (Typename tyn) = tyn
---------------
-- Variables --
---------------
data Typevar = Typevar String
derive instance eqTypevar :: Eq Typevar 
derive instance ordTypevar :: Ord Typevar
instance Show Typevar where 
  show (Typevar tyv) = tyv 

data Variable = Variable String
derive instance eqVariable :: Eq Variable 
derive instance ordVariable :: Ord Variable
instance Show Variable where 
  show (Variable v) = v 

data PrdCns = Prd | Cns 
derive instance eqPrdCns :: Eq PrdCns
derive instance ordPrdCns :: Ord PrdCns
instance Show PrdCns where 
  show Prd = "prd"
  show Cns = "cns"

data Kindvar = Kindvar String
derive instance eqKindvar :: Eq Kindvar
derive instance ordKindvar :: Ord Kindvar
instance Show Kindvar where 
 show (Kindvar kv) = kv 

--------------------------------------------------------
-------------- Classes for Free Variables --------------
--------------------------------------------------------

freshVarN :: forall a.Eq a => Ord a => Int -> String -> (String -> a) -> Set a -> a 
freshVarN n prefix ctor vars = let newV = ctor (prefix <> show n) in if newV `member` vars then freshVarN (n+1) prefix ctor vars else newV

--------------
-- Variance --
--------------
data Variance = Covariant | Contravariant
derive instance eqVariance :: Eq Variance
derive instance ordVariance :: Ord Variance
instance Show Variance where 
  show Covariant = "+"
  show Contravariant = "-"

data VariantVar = VariantVar { variantVar :: Typevar, variantVariance :: Variance}
derive instance eqVariantVar :: Eq VariantVar
derive instance ordVariantVar :: Ord VariantVar
instance Show VariantVar where 
  show (VariantVar var) = show var.variantVariance <> ":" <> show var.variantVar

-----------
-- Kinds --
-----------
data EvaluationOrder = CBV | CBN
derive instance eqEvaluationOrder :: Eq EvaluationOrder
derive instance ordEvaluationOrder :: Ord EvaluationOrder 
instance Show EvaluationOrder where 
  show CBV = "CBV"
  show CBN = "CBN"

data Kind = MkKind EvaluationOrder | MkKindVar Kindvar 
derive instance eqKind :: Eq Kind 
derive instance ordKind :: Ord Kind
instance Show Kind where 
  show (MkKind p) = show p
  show (MkKindVar kv) = show kv
class ShiftEvalOrder a where
 shiftEvalOrder :: a -> a  

instance ShiftEvalOrder EvaluationOrder where 
  shiftEvalOrder CBV = CBN
  shiftEvalOrder CBN = CBV

instance ShiftEvalOrder Kind where 
  shiftEvalOrder (MkKind eo) = MkKind $ shiftEvalOrder eo
  shiftEvalOrder v@(MkKindVar _)  = v

class ContainsKindvar a where 
  containsKindvar :: a -> Boolean

instance ContainsKindvar Kind where 
  containsKindvar (MkKind _) = false
  containsKindvar (MkKindVar _) = true


class GetKind a where 
  getKind :: a -> Kind

varianceEvalOrder :: Variance -> EvaluationOrder -> EvaluationOrder 
varianceEvalOrder Covariant = identity
varianceEvalOrder Contravariant = shiftEvalOrder

-----------------
-- Data/Codata --
-----------------
data DeclTy = Data | Codata
derive instance eqDeclTy :: Eq DeclTy 
derive instance ordDeclTy :: Ord DeclTy
instance Show DeclTy where 
  show Data = "data"
  show Codata = "codata"

defaultEo :: DeclTy -> EvaluationOrder 
defaultEo Data = CBV
defaultEo Codata = CBN

