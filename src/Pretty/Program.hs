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
import Data.Map qualified as M


instance Show P.XtorSig where 
  show (P.MkXtorSig nm [])   = show nm
  show (P.MkXtorSig nm args) = show nm <> "(" <> intercalate ", " (show <$> args) <> ")"
instance Show D.XtorSig where 
  show = show . (embed :: D.XtorSig -> P.XtorSig)
instance Show T.XtorSig where
  show = show . (embed :: T.XtorSig -> P.XtorSig)

instance Show P.DataDecl where 
  show (P.MkData n args pol sigs) =  
    "data " <> show n <> "(" <> intercalate ", " (show <$> args) <> ") : " <> show pol <> " {" <> intercalate ",  " (show <$> sigs) <> "}"
instance Show D.DataDecl where 
  show = show . (embed :: D.DataDecl -> P.DataDecl)
instance Show T.DataDecl where 
  show = show . (embed :: T.DataDecl -> P.DataDecl)

instance Show P.VarDecl where
  show (P.MkVar n t) = show n <> " := " <> show t <> ";"
instance Show D.VarDecl where 
  show = show . (embed :: D.VarDecl -> P.VarDecl) 
instance Show T.VarDecl where 
  show = show . (embed :: T.VarDecl -> P.VarDecl)

instance Show P.AnnotDecl where 
  show (P.MkAnnot n ty pol) = show n <> " :: " <> show ty <> " : " <> show pol

instance Show P.Program where 
  show (P.MkProgram decls vars annots) = show (snd <$> M.toList decls) <> "\n" <> show (snd <$> M.toList annots) <> "\n" <> show (snd <$> M.toList vars)
instance Show D.Program where 
  show = show . (embed :: D.Program -> P.Program)
instance Show T.Program where 
  show = show . (embed :: T.Program -> P.Program)
