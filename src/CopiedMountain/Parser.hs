module CopiedMountain.Parser where

import Control.Monad (void)
import Data.Void
import Data.Bifunctor (first)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import CopiedMountain.Data.AST
import Data.Text (Text, pack)
import Data.List (isPrefixOf, foldl')
import Data.Char (isDigit, isAlphaNum, isSpace)
import qualified Data.Text as Text
import Data.Functor (($>))

parseFile :: FilePath -> IO Exp
parseFile fp = do
  fc <- readFile fp
  let result = parseExpr fc
  case result of
    Left err_str -> error err_str
    Right term -> return term

parseExpr :: String -> Either String Exp
parseExpr = parseExprWith pExpr

parseExprWith :: Parser a -> String -> Either String a
parseExprWith p s = do
  let fullConsumeP = between sc eof p
  let result = runParser fullConsumeP "" s
  case result of
    Left parse_error -> Left (errorBundlePretty parse_error)
    Right result -> Right result

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.decimal

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

reservedIds :: [String]
reservedIds = [
  -- "is",
  "true",
  "false"
  -- "assert",
  -- "assertFail",
  -- "import"
  -- "Char",
  -- "String",
  -- "Int",
  -- "Float"
  ]

restrictedIdChars :: [Char]
-- restrictedIdChars = "!~?$*#.,=:;@`[]{}()<>|&']-\\%"
restrictedIdChars = "-=;`[]{}()<>#\".:"

identifier :: Parser Id
identifier = do
    name <- some $ satisfy isValidChar
    if name `elem` reservedIds
      then fail $ "Cannot use " ++ name ++ " as it is a reserved keyword"
    else if "_" `isPrefixOf` name
      then fail $ "Cannot start varname with underscore like " ++ name
    else return $ pack name
    where
      isValidChar :: Char -> Bool
      isValidChar c = not (c `elem` restrictedIdChars || isSpace c)

pExpr :: Parser Exp
pExpr = do
  res <- sepBy exprAtom (optional sc)
  case res of
    [] -> fail "unable to parse anything"
    other -> return $ foldApps other

foldApps :: [Exp] -> Exp
foldApps [exp] = exp
foldApps xs = foldl1 EApp xs

exprAtom :: Parser Exp
exprAtom =
      parens pExpr
  <|> try pLet
  <|> try pLambda
  <|> try (ELit <$> pLiteral)
  <|> EVar <$> identifier

pLet :: Parser Exp
pLet = do
  var <- identifier
  _ <- sc
  void (symbol "=")
  _ <- sc
  expr1 <- pExpr
  _ <- rword ";" <* sc
  ELet var expr1 <$> pExpr

pPattern :: Parser Pattern
pPattern = (PLit <$> pLiteral) <|> (PVar <$> identifier)

pLiteral :: Parser Lit
pLiteral = LInt <$> integer
      <|> (symbol "true" $> LBool True)
      <|> (symbol "false" $> LBool False)

pLambda :: Parser Exp
pLambda = do
  var <- pPattern
  _ <- sc
  symbol "->"
  _ <- sc
  ELam var <$> pExpr
