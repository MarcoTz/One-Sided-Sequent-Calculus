module Pretty.Terms where 

import Syntax.Untyped.Terms qualified as S 
import Syntax.Typed.Terms qualified as T 
import EmbedTyped 
import Pretty.Common ()

import Data.List (intercalate)

instance Show S.Term where 
  show (S.Var v) = v
  show (S.Mu v cmd) = "mu " <> v <> ". " <> show cmd
  show (S.Xtor xt []) = xt
  show (S.Xtor xt args) = xt <> "(" <> intercalate ", " (show <$> args) <> ")"
  show (S.XCase pts) = "case {" <>  intercalate ", " (show <$> pts) <> "}"
  show (S.Shift t) = "{" <> show t <> "}"
  show (S.Lam v cmd) = "Lambda {" <> v <> "}." <> show cmd
instance Show T.Term where 
  show t = show $ (embed :: T.Term -> S.Term) t 

instance Show S.Pattern where 
  show S.MkPattern{S.ptxt=xt, S.ptv=vars, S.ptcmd=cmd} = xt <> "(" <> intercalate ", " (show <$> vars) <> ") => " <> show cmd
instance Show T.Pattern where 
  show t = show $ (embed :: T.Pattern -> S.Pattern) t 

instance Show S.Command where 
  show (S.Cut t1 pol t2) = "<" <> show t1 <> " | " <> show pol <> " | " <> show t2 <> ">"
  show S.Done = "Done"

instance Show T.Command where 
  show c = show $ (embed :: T.Command -> S.Command) c
