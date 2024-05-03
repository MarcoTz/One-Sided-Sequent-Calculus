module SolveConstraints.Definition (
  SolverM,
  SolverState,
  runSolveM,
  ConstrTy (..),
  addSlvTyVar,
  getSlvTyVars,
  getNextConstr,
  addConstraintsArgs,
  addConstraint,
  showSubst
) where 

import SolveConstraints.Errors (SolverError(..))
import Constraints (ConstraintSet, Constr(..))
import Common (Typevar,Typename)
import Syntax.Typed.Types (Ty)

import Prelude (bind,pure, ($),(<$>),(<>),show)
import Data.Map (Map,empty,insert,toUnfoldable)
import Data.Tuple (Tuple(..))
import Data.List (List(..),intercalate)
import Data.Unit (Unit,unit)
import Data.Either (Either(..))
import Data.Maybe (Maybe (..))
import Control.Monad.Except (Except, runExcept,throwError)
import Control.Monad.State (StateT, runStateT, gets, modify)
--
-- Solver Monad 
-- 
data SolverState = MkSolverState 
  { 
  slvTyVars  :: (Map Typevar Ty), 
  remConstrs :: ConstraintSet 
}

data ConstrTy = Eq | Neq

initialSolverState :: ConstraintSet -> SolverState
initialSolverState constrs= MkSolverState {slvTyVars:empty , remConstrs:constrs}

type SolverM a = StateT SolverState (Except SolverError) a 

runSolveM :: forall a.ConstraintSet -> SolverM a -> Either SolverError (Tuple a (Map Typevar Ty))
runSolveM constrs m = case runExcept (runStateT m (initialSolverState constrs) ) of 
  Left err -> Left err 
  Right (Tuple x (MkSolverState st)) -> Right (Tuple x st.slvTyVars)

showSubst :: Map Typevar Ty -> String
showSubst varmap = do
  let substList :: List (Tuple Typevar Ty) 
      substList = toUnfoldable varmap
  let shownSubsts :: List String
      shownSubsts = (\(Tuple v ty) -> show v <> " -> " <> show ty) <$> substList
  intercalate "\n\t" shownSubsts

getNextConstr :: SolverM (Maybe Constr)
getNextConstr = do 
  constrs <- gets (\(MkSolverState s) -> s.remConstrs)
  case constrs of 
    Nil -> pure Nothing 
    (Cons c1 ctrs) -> do 
      _ <- modify (\(MkSolverState s)  -> MkSolverState s {remConstrs=ctrs} )
      pure (Just c1)

getSlvTyVars :: SolverM (Map Typevar Ty)
getSlvTyVars = gets (\(MkSolverState st) -> st.slvTyVars)

addSlvTyVar :: Typevar -> Ty -> SolverM Unit
addSlvTyVar v ty = do 
  _ <- modify $ (\(MkSolverState st) -> MkSolverState st {slvTyVars=insert v ty st.slvTyVars})
  pure unit

addConstraint :: Constr -> SolverM Unit
addConstraint constr = do
  constrs <- gets (\(MkSolverState st) -> st.remConstrs)
  _ <- modify (\(MkSolverState s) -> MkSolverState s{remConstrs=Cons constr constrs} )
  pure unit

addConstraintsArgs :: Typename -> List Ty -> List Ty -> SolverM Unit
addConstraintsArgs _ Nil Nil = pure unit 
addConstraintsArgs tyn Nil _ = throwError (ErrTyArity tyn)
addConstraintsArgs tyn _ Nil = throwError (ErrTyArity tyn)
addConstraintsArgs tyn (Cons ty1 tys1) (Cons ty2 tys2) = do
  _ <- addConstraint (MkTyEq ty1 ty2)
  _ <- addConstraintsArgs tyn tys1 tys2
  pure unit 

