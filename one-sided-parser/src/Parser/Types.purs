module Parser.Types (
  parseKindedTy,
  parseTy,
  parseTyArgs
) where 

import Common (VariantVar)
import Syntax.Parsed.Types (Ty(..), KindedTy(..))
import Parser.Definition (SrcParser)
import Parser.Lexer (parseSymbol, parseKeyword, sc, parseCommaSep)
import Parser.Symbols (Sym(..))
import Parser.Keywords (Keyword(..))
import Parser.Common (parseTypevar, parseTypename, parseVariantVar, parseKind)

import Prelude (bind, pure, ($))
import Data.List (List(..))
import Parsing.String.Basic (space)
import Parsing.Combinators (try, sepBy, many1)
import Control.Alt ((<|>))

parseTy :: SrcParser Ty 
parseTy = --(do parseTyParens) <|> parseTyForall <|> parseTyShift <|> parseTyCo <|> try parseTyDecl <|> parseTyvar
  -- parens
  (do 
  _ <- parseSymbol SymParensO
  _ <- sc
  ty <- parseTy
  _ <- sc
  _ <- parseSymbol SymParensC
  pure ty)
  <|>
  -- forall
  (do 
  _ <- parseKeyword KwForall <|> parseKeyword Kwforall 
  _ <- sc
  args <- parseTypevar `sepBy` (many1 space)
  _ <- sc
  _ <- parseSymbol SymDot
  _ <- sc
  ty <- parseTy 
  pure $ TyForall args ty)
  <|>
  -- declared type
  try ( do 
  tyn <- parseTypename 
  _ <- parseSymbol SymParensO 
  args <- parseTy `sepBy` parseCommaSep 
  _ <- parseSymbol SymParensC
  pure (TyDecl tyn args))
  <|>
  -- type var
  (do 
    var <- parseTypevar
    pure $ TyVar var )
  <|>
  -- shift
  (do
  _ <- parseSymbol SymBrackO
  _ <- sc
  ty <- parseTy
  _ <- sc
  _ <- parseSymbol SymBrackC 
  pure (TyShift ty))
  <|>
  -- co type
  (do 
  _ <- parseKeyword KwCo <|> parseKeyword Kwco
  _ <- sc
  ty <- parseTy 
  pure $ TyCo ty )
  
parseTyArgs :: SrcParser (List VariantVar)
parseTyArgs = (do 
  _ <- parseSymbol SymParensO
  vars <- parseVariantVar `sepBy` parseCommaSep 
  _ <- parseSymbol SymParensC
  pure vars)
  <|>
  pure Nil

parseKindedTy :: SrcParser KindedTy 
parseKindedTy = do 
  ty <- parseTy
  _ <- sc
  _ <- parseSymbol SymColon
  _ <- sc
  knd <- parseKind
  pure $ KindedTy {kindedTy:ty,kindedKind:knd}
