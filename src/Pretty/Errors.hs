module Pretty.Errors where 

import Errors 
import Pretty.Types () 
import Pretty.Terms ()

import Data.List (intercalate)

instance Show Error where 
  show (ErrBadPattern xts wh)           = "Malformed pattern " <> intercalate ", " (show <$> xts) <> " " <> wh
  show (ErrXtorArity xtn wh)            = " Wrong number of arguments for xtor " <> show xtn  <> " " <> wh
  show (ErrTyArity ty wh)               = "Wrong number of arguments for " <> show ty <>  " " <> wh
  show (ErrKind ShouldEq wh)            = "Kinds are not equal " <> wh
  show (ErrKind ShouldNeq wh)           = "Kinds are not different " <> wh
  show (ErrTypeNeq ty1 ty2 wh)          = show ty1 <> " != " <> show ty2 <> " " <> wh
  show (ErrMissingDecl tyn wh)          = "Type " <> show tyn <> " was not defined " <> wh
  show (ErrDuplDecl tyn wh)             = "Type " <> show tyn <> " was defined multiple times " <> wh
  show (ErrMissingVar v wh)             = "Variable " <> show v <> " was not defined " <> wh
  show (ErrDuplVar v wh)                = "Variable " <> show v <> " was defined multiple times " <> wh
  show (ErrMissingXtor xtn wh)          = "Xtor " <> show xtn <> " was not defined "  <> wh
  show (ErrDuplXtor xtn wh)             = "Xtor " <> show xtn <> " was defined multiple times "  <> wh
  show (ErrParser err)                  = "Parser Error " <> err
  show (ErrMissingXtorPt xtn wh)        = "Xtor " <> show xtn <> " not found in patterns "  <> wh
  show (ErrMissingTyVar tyv wh)         = "Type Variable " <> show tyv <> " should not appear free " <> wh
  show (ErrNotTyDecl tyn ty wh)         = "Type needs to be " <> show tyn <> " but was " <>  show ty <> " instead " <> wh
  show (ErrNotTyShift ty wh)            = "Type " <> show ty <> " should be a Shift " <> wh
  show (ErrTypeAmbig t wh)              = "Type of term " <> show t <> " is unclear "  <> wh
  show (ErrMissingType str)             = "Missing type: " <> str
