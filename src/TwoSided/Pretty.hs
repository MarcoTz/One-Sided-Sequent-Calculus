module TwoSided.Pretty where 

import Data.List 

import TwoSided.Program
import TwoSided.Syntax
import TwoSided.Types

--Program
instance Show Declaration where 
  show MkDeclaration{declName = nm, declTypeArgs=args, declPol = pol, declSig=sig}=
    "data " <> show nm <> "(" <> intercalate ", " ( (\(v,p) -> show v <> ":" <> show p) <$> args) <> ") : " <> show pol <> " where " <> show sig
  show MkCodeclaration{codeclName=nm, codeclTypeArgs=args, codeclPol=pol, codeclInterface=inter} = 
    "codata " <> show nm <> "(" <> intercalate ", " ((\(v,p) -> show v <> ":" <> show p) <$> args) <> ") : " <> show pol <> " where " <> show inter
  show MkVal{} = ""
  show MkCoval{} = ""
  show MkRec{} = ""
  show MkCorec{} = ""
  show Epsilon = ""
  show (DeclCons _ _) = ""


instance Show Signature where 
  show MkSig{sigCons = ct, sigProdArgs = args, sigConsArgs = coargs} = 
    "{" <> show ct <> ": " <> intercalate ", " (show <$> args) <> ";" <> intercalate ", " (show <$> coargs) <> "}"

instance Show Interface where 
  show MkInter{interDest=dt, interProdArgs=args, interConsArgs=coargs} = 
    "{" <> show dt <> ": (" <> intercalate ", " (show <$> args) <> ";" <> intercalate ", " (show <$> coargs) <> "}"


-- Syntax
instance Show Pol where 
  show Pos = "+"
  show Neg = "-"


instance Show Command where 
  show (Cut p pm c) = "< " <> show p <> " | " <> show pm <> " | " <> show c <> " >"

instance Show a => Show (Pattern a) where 
  show MkPattern{xtor=xt, patvars=vars, patcovars=covars, patcmd=cmd} = 
    show xt <> "(" <> intercalate ", " (show <$> vars) <> "; " <> intercalate ", " (show <$> covars) <> ")"
    <> " => " <> show cmd

instance Show Producer where 
  show (Var v) = show v
  show (Mu cv cmd) = "Mu " <> show cv <> ". " <> show cmd
  show (Constr ct prodargs consargs) = 
    show ct <> "( " <> intercalate ", " (show <$> prodargs) <> "; " <> intercalate ", " (show <$> consargs) <> ")"
  show (Cocase pats) = "Cocase {"  <> intercalate ", " (show <$> pats) <> "}"
  show (ShiftCBN p) = "{" <> show p <> "}"
  show (Lambda cv cmd) = "Lambda " <> show cv <> "." <> show cmd

instance Show Consumer where 
  show (Covar cv) = show cv
  show (MuTilde v cmd) = "TildeMu " <> show v <> ". " <> show cmd 
  show (Destr dt prodargs consargs) = show dt <> "( " <> intercalate ", " (show <$> prodargs) <> "; " <> intercalate ", " (show <$> consargs) <> ")"
  show (Case pts) = "Case { "<> intercalate ", " (show <$> pts) <> "}"
  show (ShiftCBV c) = "{" <> show c <> "}"
  show (LambdaBar v cmd) = "BarLambda " <> v <> ". " <> show cmd

-- Types
instance Show Type where 
  show (TyVar v) = show v
  show (TyDeclared nm args) = show nm <> "(" <> intercalate ", " (show <$> args) <> ")"
  show (TyDown ty) = "down( " <> show ty <> ")"
  show (TyUp ty) = "up( " <> show ty <> ")"
