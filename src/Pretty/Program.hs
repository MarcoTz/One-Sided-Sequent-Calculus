module Pretty.Program where 

import Syntax.Parsed.Program    qualified as P
import Syntax.Desugared.Program qualified as D
import Syntax.Typed.Program     qualified as T
import Embed.Definition
import Embed.EmbedTyped  () 
import Embed.EmbedDesugared () 
import Pretty.Common ()
import Pretty.Types ()
import Pretty.Terms ()

import Data.List (intercalate)


instance Show P.XtorSig where 
  show (P.MkXtorSig nm [])   = nm
  show (P.MkXtorSig nm args) = nm <> "(" <> intercalate ", " (show <$> args) <> ")"
instance Show D.XtorSig where 
  show = show . (embed :: D.XtorSig -> P.XtorSig)
instance Show T.XtorSig where
  show = show . (embed :: T.XtorSig -> P.XtorSig)

instance Show P.DataDecl where 
  show (P.MkDataDecl n args pol sigs) =  
    "data " <> n <> "(" <> intercalate ", " ((\(v,p) -> v <> show p) <$> args) <> ") : " <> show pol <> " {" <> intercalate ",\n\t " (show <$> sigs) <> "}"
instance Show D.DataDecl where 
  show = show . (embed :: D.DataDecl -> P.DataDecl)
instance Show T.DataDecl where 
  show = show . (embed :: T.DataDecl -> P.DataDecl)

instance Show P.VarDecl where 
  show (P.MkVarDecl n t) = "val " <> n <> " = " <> show t
instance Show D.VarDecl where 
  show = show . (embed :: D.VarDecl -> P.VarDecl)  
instance Show T.VarDecl where 
  show = show . (embed :: T.VarDecl -> P.VarDecl)
  
instance Show P.Program where 
  show (P.MkProgram decls terms) = "Type Declarations \n\t" <> intercalate ",\n\t" (show <$> decls) <> "\n Term Declarations\n\t" <> intercalate ",\n\t" (show <$> terms)
instance Show D.Program where 
  show = show . (embed :: D.Program -> P.Program)
instance Show T.Program where 
  show = show . (embed :: T.Program -> P.Program)
