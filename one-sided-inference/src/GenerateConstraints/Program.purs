module GenerateConstraints.Program (
  genConstraintsVarDecl
  )
where 

import Prelude (bind,($),pure)

import GenerateConstraints.Definition (GenM)
import GenerateConstraints.Terms (genConstraintsTerm)
import Syntax.Desugared.Program (VarDecl(..)) as D
import Syntax.Typed.Program (VarDecl(..)) as T

genConstraintsVarDecl :: D.VarDecl -> GenM T.VarDecl
genConstraintsVarDecl (D.VarDecl var) = do
  bd' <- genConstraintsTerm var.varBody
  pure $ T.VarDecl {varPos:var.varPos, varIsRec:var.varIsRec, varName:var.varName, varBody:bd'}
