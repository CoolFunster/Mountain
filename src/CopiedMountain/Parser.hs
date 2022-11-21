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
import Control.Monad.Combinators.Expr
import qualified Data.Map as M
import Debug.Trace
import CopiedMountain.Hash

parseFile :: FilePath -> IO Exp
parseFile fp = do
  fc <- readFile fp
  let result = parseExpr fc
  case result of
    Left err_str -> error err_str
    Right term -> return term

parseExpr :: String -> Either String Exp
parseExpr = parseExprWith (between sc eof pExpr)

parseExprWith :: Parser a -> String -> Either String a
parseExprWith p s = do
  let result = runParser p "" s
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

binaryL  inner_parser f = InfixL  (f <$ inner_parser)
binaryR  inner_parser f = InfixR  (f <$ inner_parser)
binaryN  inner_parser f = InfixN  (f <$ inner_parser)
prefix  inner_parser f = Prefix  (f <$ inner_parser)
postfix inner_parser f = Postfix (f <$ inner_parser)

reservedIds :: [String]
reservedIds = [
  "true",
  "false",
  "Bool",
  "Char",
  "String",
  "Int",
  "Float",
  "Thing",
  "type"
  ]

restrictedIdChars :: [Char]
-- restrictedIdChars = "!~?$*#.,=:;@`[]{}()<>|&']-\\%"
restrictedIdChars = "|-=;`[]{}()<>#\".:,~*"

identifier :: Parser Id
identifier = do
    name <- some $ satisfy isValidChar
    if name `elem` reservedIds
      then fail $ "Cannot use " ++ name ++ " as it is a reserved keyword"
    else if "_" `isPrefixOf` name
      then fail $ "Cannot start varname with underscore like " ++ name
    else return name
    where
      isValidChar :: Char -> Bool
      isValidChar c = not (c `elem` restrictedIdChars || isSpace c)

pWrapWS :: [Char] -> Parser String
pWrapWS input_str = try $ between (optional sc) (optional sc) (symbol input_str)

pExpr :: Parser Exp
pExpr = pBinExp <* optional (symbol ".")

pBinExp :: Parser Exp
pBinExp = makeExprParser pAnnot [
    [binaryR (try pColon) (varLTerm ELabel)],
    [prefix (symbol "*") (EUnique Unset)],
    [binaryR (try $ pWrapWS "~") (varLTerm ERec)],
    [binaryR (try $ pWrapWS "-->") (ELam . expAsPattern),
     binaryR (try $ pWrapWS "->") (EULam . expAsPattern)],
    [binaryR (try $ pWrapWS "||") EMatch]
  ]
  where
    varLTerm a (EVar x) rhs = a x rhs
    varLTerm a other rhs = error "ids must be on the lhs"

    pColon = optional sc *> symbol ":" <* notFollowedBy (symbol ":") <* optional sc

pAnnot :: Parser Exp
pAnnot = do
  typ <- optional (try (pType <* pWrapWS "::"))
  case typ of
    Just st -> EAnnot st <$> pExpr
    Nothing -> pCall

pCall :: Parser Exp
pCall = do
  struct1 <- pExprAtom
  struct2 <- many $ try (optional sc *> (pCallInfix <|> pCallNormal))
  return $ foldl' (\state f -> f state) struct1 struct2

pCallInfix :: Parser (Exp -> Exp)
pCallInfix = do
  struct <- between (symbol "`") (symbol "`") pExprAtom
  return $ EApp struct

pCallNormal :: Parser (Exp -> Exp)
pCallNormal = do
  struct <- try pExprAtom
  return (`EApp` struct)

pExprAtom :: Parser Exp
pExprAtom =
      try pTuple
  <|> try pLet
  <|> try pTDef
  <|> try (ELit <$> pLiteral)
  <|> EVar <$> identifier

pLet :: Parser Exp
pLet = do
  _ <- symbol "def" <* sc
  var <- pPattern
  b <- choice [try $ pWrapWS "*=", try $ pWrapWS "="]
  expr1 <- pExpr
  _ <- rword ";" <* sc
  case b of
    "*=" -> EULet var expr1 <$> pExpr
    "=" -> ELet var expr1 <$> pExpr
    other -> error "should never reach here"

pTDef :: Parser Exp
pTDef = do
  _ <- symbol "type" <* sc
  n <- identifier
  _ <- pWrapWS "="
  expr1 <- pType
  _ <- symbol ";" <* sc
  ETDef n expr1 <$> pExpr

pTuple :: Parser Exp
pTuple = do
  _ <- symbol "(" <* optional sc
  inner <- sepEndBy pExpr (pWrapWS ",")
  _ <- optional sc *> symbol ")"
  case inner of
    [] -> return $ ELit LUnit
    [x] -> return x
    other -> return $ _fold other
    where
      _fold :: [Exp] -> Exp
      _fold [x,y] = EPair x y
      _fold (x:xs) = EPair x (_fold xs)
      _fold other = error "should not reach"

pPattern :: Parser Pattern
pPattern = expAsPattern <$> pExpr

pLiteral :: Parser Lit
pLiteral = choice [
  pThing,
  pBool,
  pChar,
  pString,
  try pFloat,
  pInt
  ]

pThing :: Parser Lit
pThing = symbol "#" *> (LThing <$> identifier)

pChar :: Parser Lit
pChar = LChar <$> between (symbol "\'") (symbol "\'") (try escaped <|> normalChar)
   where
     escaped :: Parser Char
     escaped = do {
        _ <- symbol "\\"
      ; satisfy (=='\'')
     }
     normalChar :: Parser Char
     normalChar = satisfy (/= '\'')

pString :: Parser Lit
pString = LString <$> between (symbol "\"") (symbol "\"") (many (try escaped <|> normalChar))
   where
     escaped :: Parser Char
     escaped = do {
        _ <- symbol "\\"
      ; satisfy (=='\"')
     }
     normalChar :: Parser Char
     normalChar = satisfy (/= '\"')

pBool :: Parser Lit
pBool = LBool <$> do
  res <- symbol "true" <|> symbol "false"
  case res of
    "true" -> return True
    "false" -> return False
    _ -> error "unreachable"

pInt :: Parser Lit
pInt = LInt <$> L.signed (L.space Text.Megaparsec.empty Text.Megaparsec.empty Text.Megaparsec.empty) L.decimal

pFloat :: Parser Lit
pFloat = LFloat <$> L.signed sc L.float

pType :: Parser Type
pType = pBinTyp

pBinTyp :: Parser Type
pBinTyp = makeExprParser pTCall [
    [binaryR (try pColon) pLabel],
    [prefix (try $ symbol "*") TUnique],
    [binaryR (try $ pWrapWS "-->") TFun,
     binaryR (try $ pWrapWS "->") TUFun],
    [binaryR (try $ pWrapWS "|") TSum]
  ]
  where
    pLabel (TVar x) rhs = TLabel x rhs
    pLabel other rhs = error "labels can only have ids on the lhs"

    pColon = optional sc *> symbol ":" <* notFollowedBy (symbol ":") <* optional sc

pTCall :: Parser Type
pTCall = do
  struct1 <- pTypeAtom
  struct2 <- many $ try (optional sc *> (pTCallInfix <|> pTCallNormal))
  return $ foldl' (\state f -> f state) struct1 struct2

pTCallInfix :: Parser (Type -> Type)
pTCallInfix = do
  struct <- between (symbol "`") (symbol "`") pTypeAtom
  return $ TCall struct

pTCallNormal :: Parser (Type -> Type)
pTCallNormal = do
  struct <- try pTypeAtom
  return (`TCall` struct)

pTypeAtom :: Parser Type
pTypeAtom =
      pBuiltInType
  <|> pTTuple
  <|> pTVar
  <|> TType <$> try pKind

pBuiltInType :: Parser Type
pBuiltInType = choice [
    symbol "Int" $> TInt,
    symbol "Bool" $> TBool,
    symbol "Char" $> TChar,
    symbol "String" $> TString,
    symbol "Float" $> TFloat,
    symbol "Thing" $> TThing
  ]

pTVar :: Parser Type
pTVar = TVar <$> identifier

pTTuple :: Parser Type
pTTuple = do
  _ <- symbol "(" <* optional sc
  inner <- sepEndBy pType (pWrapWS ",")
  _ <- optional sc *> symbol ")"
  case inner of
    [] -> return TUnit
    [x] -> return x
    other -> return $ _fold other
    where
      _fold :: [Type] -> Type
      _fold [x,y] = TPair x y
      _fold (x:xs) = TPair x (_fold xs)
      _fold other = error "should not reach"

pKind :: Parser Kind
pKind = pBinKind

pBinKind :: Parser Kind
pBinKind = makeExprParser pKindAtom [
    [binaryR (optional sc) KApp],
    [binaryR (try $ pWrapWS "-->") KFun]
  ]

pKindAtom :: Parser Kind
pKindAtom = symbol "Type" $> KType
  <|> parens pKindAtom
