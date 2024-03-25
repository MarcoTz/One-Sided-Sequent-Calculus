module TypeCheck.Errors (
  CheckerError (..)
) where 

import Common 
import Errors
import Loc
import Syntax.Typed.Types
import Syntax.Typed.Terms     qualified as T
import Syntax.Desugared.Terms qualified as D
import Pretty.Common ()
import Pretty.Typed ()
import Pretty.Desugared()

import Data.List (intercalate)

data CheckerError =
  ErrNoAnnot !Variable
  | ErrUndefinedVar !Variable
  | ErrUndefinedTyVar !TypeVar !D.Term
  | ErrFreeTyVar !TypeVar
  | ErrTyCoForShift !T.Term !Ty
  | ErrKindNeq !Ty !Ty !D.Term
  | ErrTypeNeq !Ty !Ty !D.Term
  | ErrNotTyDecl !TypeName !Ty !D.Term
  | ErrTypeArity !TypeName
  | ErrXtorArity !XtorName
  | ErrBadPattern ![XtorName] ![XtorName] !D.Term
  | ErrCutKind !Ty !Ty !D.Command
  | ErrBadType !D.Term !Ty
  | ErrUnclearTypeCut !D.Term !D.Term
  | ErrEnv !String !Loc


whileTerm :: D.Term -> String 
whileTerm t = "while type checking " <> show t
whileCmd :: D.Command -> String 
whileCmd c = "while type checking " <> show c

instance Error CheckerError where 
  getMessage (ErrNoAnnot var) = "No annotation for " <> show var <> ", cannot type check."
  getMessage (ErrUndefinedVar var) = "Variable " <> show var <> " was not defined "
  getMessage (ErrUndefinedTyVar tyv t) = "Type Variable " <> show tyv <> " was not defined " <> whileTerm t
  getMessage (ErrFreeTyVar tyv) = "Type Variable " <> show tyv <> " cannot appear free"
  getMessage (ErrTyCoForShift t ty) = "Cannot use co-type of " <> show ty <> " for shift term " <> show t
  getMessage (ErrKindNeq ty1 ty2 t) = "Kinds of types " <> show ty1 <> " and " <> show ty2 <> " are not equal"  <> whileTerm t
  getMessage (ErrTypeNeq ty1 ty2 t) = "Types " <> show ty1 <> " and " <> show ty2 <> " should be equal " <> whileTerm t
  getMessage (ErrNotTyDecl tyn ty t) = "Type " <> show ty <> " should be " <> show tyn <> " " <> whileTerm t
  getMessage (ErrTypeArity tyn) = "Wrong number of arguments for type " <> show tyn
  getMessage (ErrXtorArity xtn) = "Wrong number of arguments for xtor " <> show xtn
  getMessage (ErrBadPattern exPts shouldPts t) = "Malformed case: found patterns for " <> intercalate ", " (show <$> exPts) <> ", expected " <> intercalate ", " (show <$> shouldPts) <> " " <> whileTerm t
  getMessage (ErrCutKind ty1 ty2 c) = "Kind of types " <> show ty1 <> " and " <> show ty2 <> " in cut are not equal " <> whileCmd c
  getMessage (ErrBadType t ty) = "Cannot typecheck " <> show t <> " with type " <> show ty
  getMessage (ErrUnclearTypeCut t1 t2) = "Types of terms " <> show t1 <> " and " <> show t2 <> " in cut unclear"
  getMessage (ErrEnv str _) = str

  getLoc _ = defaultLoc
  toError = ErrEnv
