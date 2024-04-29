module GenerateConstraints.Program (
  genConstraintsVarDecl
  )
where 

import Prelude (bind,($),pure)

import GenerateConstraints.Definition (GenM)
import GenerateConstraints.Terms (genConstraintsTerm)
import Syntax.Desugared.Program (VarDecl(..)) as D
import Syntax.Typed.Program (VarDecl(..)) as T
import Syntax.Typed.Terms (getType)

import Debug (trace)
import Prelude (show,unit,(<>))

genConstraintsVarDecl :: D.VarDecl -> GenM T.VarDecl
genConstraintsVarDecl (D.VarDecl var) = do
  let _ = trace ("generating constraints for var decl " <> show var.varName) (\_->unit)
  bd' <- genConstraintsTerm var.varBody
  pure $ T.VarDecl {varPos:var.varPos, varIsRec:var.varIsRec, varName:var.varName, varTy:getType bd', varBody:bd'}
