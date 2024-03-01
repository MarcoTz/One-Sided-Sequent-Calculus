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

instance Show P.Decl where 
  show (P.MkData n args pol sigs) =  
    "data " <> n <> "(" <> intercalate ", " ((\(v,p) -> v <> show p) <$> args) <> ") : " <> show pol <> " {" <> intercalate ",  " (show <$> sigs) <> "}"
  show (P.MkVar n t) = n <> " := " <> show t <> ";"
  show (P.MkAnnot n ty) = n <> " :: " <> show ty

instance Show D.Decl where 
  show = show . (embed :: D.Decl -> P.Decl)

instance Show T.DataDecl where 
  show = show . (embed :: T.DataDecl -> P.Decl)

instance Show T.VarDecl where 
  show = show . (embed :: T.VarDecl -> P.Decl)
  
instance Show T.Program where 
  show = show . (embed :: T.Program -> [P.Decl])
