module Pretty.Errors where 

import Errors 
import Pretty.Types () 
import Pretty.Terms ()

import Data.List (intercalate)

instance Show Error where 
  show (ErrBadPattern xts wh)           = "Malformed pattern " <> intercalate ", " (show <$> xts) <> " in " <> show wh
  show (ErrXtorArity xtn TooFew wh)     = " Too few arguments for xtor " <> xtn <> show wh
  show (ErrXtorArity xtn TooMany wh)    = "Too many arguments for xtor " <> xtn <> show wh
  show (ErrTyArity ty wh)               = "Wrong number of arguments for " <> ty <> show wh
  show (ErrKind knd1 knd2 ShouldEq wh)  = show knd1 <> " != " <> show knd2 <> ", should be equal " <> show wh
  show (ErrKind knd1 knd2 ShouldNeq wh) = show knd1 <> " = " <> show knd2 <> ", should be different " <> show wh
  show (ErrTypeNeq ty1 ty2 wh)          = show ty1 <> " != " <> show ty2 <> show wh
  show (ErrTypeSchemeNeq tys1 tys2 wh)  = show tys1 <> " != " <> show tys2 <> show wh 
  show (ErrMissingDecl tyn wh)          = "Type " <> tyn <> " was not defined " <> show wh
  show (ErrMissingVar v wh)             = "Variable " <> v <> " was not defined" <> show wh
  show (ErrMissingXtor xtn wh)          = "Xtor " <> xtn <> " was not defined " <> show wh
  show (ErrParser err)                  = "Parser Error " <> err
  show (ErrMissingXtorPt xtn wh)        = "Xtor " <> xtn <> " not found in patterns" <> show wh 
  show (ErrMissingTyVar tyv wh)         = "Type Variable " <> tyv <> " was should not appear free " <> show wh

instance Show ErrWhere where 
  show WhereCheck     = " (during type checking)"
  show WhereEval      = " (during evaluation)"
  show WhereInfer     = " (during type inference)"
  show WhereSolve     = " (during constraint solving)"
  show WhereEnv       = " (in environment)"
  show WhereDecl      = " (during declaration checking)"
  show WhereParser    = " (during parsing)"
  show WhereGenConstr = " (during constraint generation)"
  show WhereDesugar   = " (during desugaring)"
