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
restrictedIdChars = "|-=;`[]{}()<>#\".:,~*$+"

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
pWrapWS input_str = try $ optional sc *> symbol input_str <* notFollowedBy (symbol input_str) <* optional sc

pExpr :: Parser Exp
pExpr = pMatch

pMatch :: Parser Exp
pMatch = makeExprParser pFun [
    [binaryR (try $ pWrapWS "||") EMatch]
  ]

pFun :: Parser Exp
pFun = 
      EFun <$> try (pPattern <* pWrapWS "->") <*> pFun
  <|> pBinExp

pBinExp :: Parser Exp
pBinExp = 
      ELabel <$> try (identifier <* pWrapWS ":") <*> pBinExp
  <|> ERec <$> try (identifier <* pWrapWS "~") <*> pBinExp
  <|> pAnnot
  
pAnnot :: Parser Exp
pAnnot =
      EAnnot <$> try (pType <* pWrapWS "::") <*> pExpr
  <|> pCall

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
      try $ pTuple (ELit LUnit) EPair pExpr
  <|> try pLet
  <|> try pTDef
  <|> try (ELit <$> pLiteral)
  <|> EVar <$> identifier

pLet :: Parser Exp
pLet = do
  _ <- symbol "def" <* sc
  var <- pPattern
  _ <- try $ pWrapWS "="
  expr1 <- pExpr
  _ <- symbol ";" <* sc
  ELet var expr1 <$> pExpr

pTDef :: Parser Exp
pTDef = do
  _ <- symbol "type" <* sc
  n <- identifier
  _ <- pWrapWS "="
  expr1 <- pType
  _ <- symbol ";" <* sc
  ETDef n expr1 <$> pExpr

pTuple :: a -> (a -> a -> a) -> Parser a -> Parser a
pTuple unit pair p = do
  _ <- symbol "(" <* optional sc
  inner <- sepEndBy p (pWrapWS ",")
  _ <- optional sc *> symbol ")"
  case inner of
    [] -> return unit
    [x] -> return x
    other -> return $ _fold other
    where
      _fold [x,y] = pair x y
      _fold (x:xs) = pair x (_fold xs)
      _fold other = error "should not reach"

pPattern :: Parser Pattern
pPattern = choice [
    try $ pTuple (PLit LUnit) PPair pPattern,
    PLabel <$> try (identifier <* pWrapWS ":") <*> pPattern,
    PAnnot <$> try (pUsageType <* pWrapWS "::") <*> pPattern,
    PLit <$> try pLiteral,
    PWildcard <$ symbol "?",
    PVar <$> try identifier
  ]

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

pUseCount :: Parser UseCount
pUseCount = choice [
    try $ symbol "+" $> CMany,
    try $ symbol "?" $> CSingle,
    try $ symbol "*" $> CAny,
    return CAny
  ]

pUsageType :: Parser (Usage,Type)
pUsageType = pUseSum

pUseSum :: Parser (Usage, Type)
pUseSum = do
  inner <- sepBy1 pUseTerm (pWrapWS "|")
  return $ _fold inner
  where
    _fold :: [(Usage, Type)] -> (Usage, Type)
    _fold [x] = x
    _fold [(ux, tx),(uy, ty)] = (USum ux uy, TSum tx ty)
    _fold ((ux, tx):xs) = do
      let (uy, ty) = _fold xs
      (USum ux uy, TSum tx ty)
    _fold other = error "should not reach"

pUseTerm :: Parser (Usage, Type)
pUseTerm = choice [
    try pUsePair,
    try pUseLiteral
  ]

pUsePair :: Parser (Usage, Type)
pUsePair = do
  count <- pUseCount
  _ <- symbol "(" <* optional sc
  inner <- sepEndBy pUsageType (pWrapWS ",")
  _ <- optional sc *> symbol ")"
  case inner of
    [] -> return (ULit count, TUnit)
    [x] -> return x
    other -> return $ _fold count other
  where
    _fold :: UseCount -> [(Usage, Type)] -> (Usage, Type)
    _fold _ [x] = x
    _fold uc [(ux, tx),(uy, ty)] = (UPair uc ux uy, TPair tx ty)
    _fold uc ((ux, tx):xs) = do
      let (uy, ty) = _fold uc xs
      (UPair uc ux uy, TPair tx ty)
    _fold uc other = error "should not reach"

pUseLiteral :: Parser (Usage, Type)
pUseLiteral = do
  count <- pUseCount
  typ <- pTypeAtom
  return (ULit count, typ)

pType :: Parser Type
pType = pTFun

pTFun :: Parser Type
pTFun = choice [
    TFun <$> try (pUsageType <* pWrapWS "->") <*> pType,
    pBinTyp
  ]

pBinTyp :: Parser Type
pBinTyp = makeExprParser pTCall [
    [binaryR (try pColon) pLabel],
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
  <|> pTToken

pTToken :: Parser Type
pTToken = do
  _ <- symbol "$"
  TToken <$> identifier

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
    [binaryR (try $ pWrapWS "->") KFun]
  ]

pKindAtom :: Parser Kind
pKindAtom = symbol "Type" $> KType
  <|> parens pKindAtom
