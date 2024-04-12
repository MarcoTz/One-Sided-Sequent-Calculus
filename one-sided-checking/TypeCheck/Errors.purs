module TypeCheck.Errors (
  CheckerError (..)
) where 

import Common 
import Errors
import Loc
import Syntax.Typed.Types     qualified as T
import Syntax.Typed.Terms     qualified as T
import Syntax.Desugared.Terms qualified as D
import Syntax.Desugared.Types qualified as D
import Pretty.Common ()
import Pretty.Typed ()
import Pretty.Desugared()

import Data.List (intercalate)

data CheckerError where 
  ErrNoAnnot        :: Loc -> Variable -> CheckerError 
  ErrUndefinedVar   :: Loc -> Variable -> CheckerError 
  ErrNotSubsumed    :: Loc -> T.Ty -> T.Ty -> CheckerError
  ErrUndefinedTyVar :: Loc -> Typevar -> D.Term-> CheckerError 
  ErrFreeTyVar      :: Loc -> Typevar-> CheckerError 
  ErrTyCoForShift   :: Loc -> T.Term ->T.Ty-> CheckerError 
  ErrKindNeq        :: Loc -> T.Ty -> T.Ty ->D.Term-> CheckerError 
  ErrKindUnclear    :: Loc -> D.Ty -> CheckerError
  ErrTypeNeq        :: Loc -> T.Ty -> T.Ty -> D.Term-> CheckerError 
  ErrNotTyDecl      :: Loc -> Typename -> T.Ty -> D.Term-> CheckerError 
  ErrTypeArity      :: Loc -> Typename-> CheckerError 
  ErrXtorArity      :: Loc -> Xtorname-> CheckerError 
  ErrBadPattern     :: Loc -> [Xtorname] -> [Xtorname] -> D.Term-> CheckerError 
  ErrCutKind        :: Loc -> T.Ty -> T.Ty -> D.Command-> CheckerError 
  ErrBadType        :: Loc -> D.Term -> T.Ty -> CheckerError 
  ErrUnclearType    :: Loc -> D.Command -> CheckerError 
  ErrList           :: Loc -> [CheckerError] -> CheckerError
  ErrOther          :: Loc -> String -> CheckerError 


whileTerm :: D.Term -> String 
whileTerm t = "while type checking " <> show t
whileCmd :: D.Command -> String 
whileCmd c = "while type checking " <> show c

instance Error CheckerError where 
  getMessage (ErrNoAnnot _ var) = "No annotation for " <> show var <> ", cannot type check."
  getMessage (ErrUndefinedVar _ var) = "Variable " <> show var <> " was not defined "
  getMessage (ErrUndefinedTyVar _ tyv t) = "Type Variable " <> show tyv <> " was not defined " <> whileTerm t
  getMessage (ErrFreeTyVar _ tyv) = "Type Variable " <> show tyv <> " cannot appear free"
  getMessage (ErrTyCoForShift _ t ty) = "Cannot use co-type of " <> show ty <> " for shift term " <> show t
  getMessage (ErrKindNeq _ ty1 ty2 t) = "Kinds of types " <> show ty1 <> " and " <> show ty2 <> " are not equal "  <> whileTerm t
  getMessage (ErrNotSubsumed _ ty1 ty2) = "Type " <> show ty1 <> " should be subsumed by " <> show ty2
  getMessage (ErrKindUnclear _ ty) = "Kind of type " <> show ty <> " is unclear" 
  getMessage (ErrTypeNeq _ ty1 ty2 t) = "Types " <> show ty1 <> " and " <> show ty2 <> " should be equal " <> whileTerm t
  getMessage (ErrNotTyDecl _ tyn ty t) = "Type " <> show ty <> " should be " <> show tyn <> " " <> whileTerm t
  getMessage (ErrTypeArity _ tyn) = "Wrong number of arguments for type " <> show tyn
  getMessage (ErrXtorArity _ xtn) = "Wrong number of arguments for xtor " <> show xtn
  getMessage (ErrBadPattern _ exPts shouldPts t) = "Malformed case: found patterns for " <> intercalate ", " (show <$> exPts) <> ", expected " <> intercalate ", " (show <$> shouldPts) <> " " <> whileTerm t
  getMessage (ErrCutKind _ ty1 ty2 c) = "Kind of types " <> show ty1 <> " and " <> show ty2 <> " in cut are not equal " <> whileCmd c
  getMessage (ErrBadType _ t ty) = "Cannot typecheck " <> show t <> " with type " <> show ty
  getMessage (ErrUnclearType _ c) = "Type of term " <> show c <> " is unclear" 
  getMessage (ErrList _ errs) = intercalate "\n " (getMessage <$> errs)
  getMessage (ErrOther _ str)  = str

  getLocation (ErrNoAnnot loc _) = loc
  getLocation (ErrUndefinedVar loc _) = loc 
  getLocation (ErrUndefinedTyVar loc _ _) = loc
  getLocation (ErrFreeTyVar loc _) = loc 
  getLocation (ErrTyCoForShift loc _ _) = loc 
  getLocation (ErrKindNeq loc _ _ _) = loc
  getLocation (ErrNotSubsumed loc _ _ ) = loc
  getLocation (ErrKindUnclear loc _) = loc
  getLocation (ErrTypeNeq loc _ _ _) = loc
  getLocation (ErrNotTyDecl loc _ _ _) = loc
  getLocation (ErrTypeArity loc _) = loc 
  getLocation (ErrXtorArity loc _) = loc 
  getLocation (ErrBadPattern loc _ _ _) = loc
  getLocation (ErrCutKind loc _ _ _) = loc 
  getLocation (ErrBadType loc _ _) = loc 
  getLocation (ErrUnclearType loc _) = loc 
  getLocation (ErrList loc _) = loc
  getLocation (ErrOther loc _) = loc

  toError = ErrOther 
