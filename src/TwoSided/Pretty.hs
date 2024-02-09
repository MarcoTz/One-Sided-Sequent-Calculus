module TwoSided.Pretty where 

import Data.List 

import TwoSided.Program
import TwoSided.Syntax
import TwoSided.Types

--Program

instance Show (TypeDeclaration a) where 
  show MkTyDecl{declName = tyn, declTyArgs = args, declPol = pol, declSig = sigs} = 
      show pol <> tyn <> "(" <> intercalate ", " ( (\(var,pol') -> show pol' <> var) <$> args) <> ") { \n\t" <> intercalate ", \n\t" (show <$> sigs) <> "\n}"

instance Show Declaration where 
  show (DataDecl decl) = "data " <> show decl
  show (CodataDecl decl) = "codata " <> show decl
  show MkVal{} = ""
  show MkCoval{} = ""
  show MkRec{} = ""
  show MkCorec{} = ""
  show Epsilon = ""
  show (DeclCons _ _) = ""

instance Show (XtorSig a) where 
  show MkXtorSig{sigName = nm, sigProdArgs = [], sigConsArgs = []} = nm
  show MkXtorSig{sigName = nm, sigProdArgs = args, sigConsArgs = coargs} = 
    nm <> "(" <> intercalate ", " (show <$> args) <> "; " <> intercalate ", " (show <$> coargs) <> ")"

--MKXtorSig {sigName :: !XtorName, sigProdArgs :: ![Type], sigConsArgs :: ![Type]} 
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
  show (Var v) = v
  show (Mu cv cmd) = "Mu " <> show cv <> ". " <> show cmd
  show (Constr ct prodargs consargs) = 
    show ct <> "( " <> intercalate ", " (show <$> prodargs) <> "; " <> intercalate ", " (show <$> consargs) <> ")"
  show (Cocase pats) = "Cocase {"  <> intercalate ", " (show <$> pats) <> "}"
  show (ShiftCBN p) = "{" <> show p <> "}"
  show (Lambda cv cmd) = "Lambda " <> show cv <> "." <> show cmd

instance Show Consumer where 
  show (Covar cv) = cv
  show (MuTilde v cmd) = "TildeMu " <> show v <> ". " <> show cmd 
  show (Destr dt prodargs consargs) = show dt <> "( " <> intercalate ", " (show <$> prodargs) <> "; " <> intercalate ", " (show <$> consargs) <> ")"
  show (Case pts) = "Case { "<> intercalate ", " (show <$> pts) <> "}"
  show (ShiftCBV c) = "{" <> show c <> "}"
  show (LambdaBar v cmd) = "BarLambda " <> v <> ". " <> show cmd

-- Types
instance Show Type where 
  show (TyVar v) = v
  show (TyDeclared nm args) = nm <> "(" <> intercalate ", " (show <$> args) <> ")"
  show (TyDown ty) = "down( " <> show ty <> ")"
  show (TyUp ty) = "up( " <> show ty <> ")"
