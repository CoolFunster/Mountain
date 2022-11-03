module SimpleMountain.SimpleMountainParser where

import SimpleMountain.SimpleMountain

import Text.Megaparsec
import Text.Megaparsec.Error
import Text.Megaparsec.Char ( space1, string )
import Text.Megaparsec.Debug (dbg)
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.ByteString as B
import qualified Data.Map as M

import Control.Monad.Combinators.Expr

import Data.Text ( Text, pack, unpack, unpack, replace)
import Data.Void ( Void )
import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.Char ( isSpace )
import Data.List ( intercalate, foldl', sort, isPrefixOf )
import Data.Functor ( ($>) )

import Debug.Trace (trace)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.FilePath.Posix

import qualified Data.Text.IO as TextIO
import qualified Data.Set as Set
import Control.Monad.Trans
import Hash


_baseParse :: Parser a -> FilePath -> Text -> Either String a
_baseParse parser file_path file_contents = do
    let result = runParser (parser <* spaceConsumer <* eof) file_path file_contents
    case result of
        Left parse_error -> Left (errorBundlePretty parse_error)
        Right result -> Right result

parseFileWith :: Parser a -> FilePath -> IO (Either String a)
parseFileWith parser path_to_file = do
    contents <- TextIO.readFile path_to_file
    let category = _baseParse parser path_to_file contents
    return category

tryParseFile :: FilePath -> IO (Either String MountainTypedTerm)
tryParseFile = parseFileWith (pTypedStructure pMountainTypes pMountainLiteral)

parseFile :: FilePath -> IO MountainTypedTerm
parseFile fp = do
  result <- parseFileWith pMountain fp
  case result of
    Left err_str -> error err_str
    Right term -> return term

parseStringWith :: Parser a -> [Char] -> Either String a
parseStringWith cat_parser input_str = _baseParse cat_parser "String" (pack input_str)

parseString :: [Char] -> Either String MountainTypedTerm
parseString = parseStringWith pMountain

parseTextWith :: Parser a -> Text -> Either String a
parseTextWith cat_parser = _baseParse cat_parser "String"

parseText :: Text -> Either String MountainTypedTerm
parseText = parseTextWith pMountain

type Parser = Parsec Void Text

-- ######################
-- ###  Helpers       ###
-- ######################
spaceConsumer :: Parser ()
spaceConsumer = L.space
                space1                         -- (2)
                (L.skipLineComment $ pack "//")       -- (3)
                (L.skipBlockComment (pack "/*") (pack "*/")) -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol x = do
  res <- L.symbol spaceConsumer (pack x)
  return $ unpack res

binaryL  inner_parser f = InfixL  (f <$ inner_parser)
binaryR  inner_parser f = InfixR  (f <$ inner_parser)
binaryN  inner_parser f = InfixN  (f <$ inner_parser)
prefix  inner_parser f = Prefix  (f <$ inner_parser)
postfix inner_parser f = Postfix (f <$ inner_parser)

pWrapWS :: [Char] -> Parser String
pWrapWS input_str = try $ between (optional spaceConsumer) (optional spaceConsumer) (symbol input_str)

{- This wraps a parser around a pair of characters -}
pWrapBetween :: [Char] -> [Char] -> Parser a -> Parser a
pWrapBetween open_char close_char = between (symbol open_char <* optional spaceConsumer) (optional spaceConsumer *> symbol close_char)
-- {- megaparsec helpers end -}The func

reservedIds :: [String]
reservedIds = [
  "is",
  "True",
  "False",
  "assert",
  "assertFail",
  "import"
  -- "Char",
  -- "String",
  -- "Int",
  -- "Float"
  ]

restrictedIdChars :: [Char]
-- restrictedIdChars = "!~?$*#.,=:;@`[]{}()<>|&']-\\%"
restrictedIdChars = "-=;`[]{}()<>#\".:"

pId :: Parser Id
pId = do
    name <- some $ satisfy isValidChar
    if name `elem` reservedIds
      then fail $ "Cannot use " ++ name ++ " as it is a reserved keyword"
    else if "_" `isPrefixOf` name
      then fail $ "Cannot start varname with underscore like " ++ name
    else return name
    where
      isValidChar :: Char -> Bool
      isValidChar c = not (c `elem` restrictedIdChars || isSpace c)

-- ########################
-- ### MountainBuiltins ###
-- ########################

pSimpleLiteral :: String -> a -> Parser a
pSimpleLiteral s l = symbol s $> l

pMountainLiteral :: Parser MountainLiteral
pMountainLiteral = choice [
  pThing,
  pBool,
  pChar,
  pString,
  try pFloat,
  pInt,
  try $ pSimpleLiteral "assertIs" AssertIs,
  try $ pSimpleLiteral "assertFail" AssertFail,
  try $ pSimpleLiteral "assert" Assert,
  try $ pSimpleLiteral "is" Is
  ]

pThing :: Parser MountainLiteral
pThing = symbol "#" *> (Thing <$> pId)

pChar :: Parser MountainLiteral
pChar = Char <$> between (symbol "\'") (symbol "\'") (try escaped <|> normalChar)
   where
     escaped :: Parser Char
     escaped = do {
        _ <- symbol "\\"
      ; satisfy (=='\'')
     }
     normalChar :: Parser Char
     normalChar = satisfy (/= '\'')

pString :: Parser MountainLiteral
pString = String <$> between (symbol "\"") (symbol "\"") (many (try escaped <|> normalChar))
   where
     escaped :: Parser Char
     escaped = do {
        _ <- symbol "\\"
      ; satisfy (=='\"')
     }
     normalChar :: Parser Char
     normalChar = satisfy (/= '\"')

pBool :: Parser MountainLiteral
pBool = Bool <$> do
  res <- symbol "True" <|> symbol "False"
  case res of
    "True" -> return True
    "False" -> return False
    _ -> error "unreachable"

pInt :: Parser MountainLiteral
pInt = Int <$> L.signed (L.space Text.Megaparsec.empty Text.Megaparsec.empty Text.Megaparsec.empty) L.decimal

pFloat :: Parser MountainLiteral
pFloat = Float <$> L.signed spaceConsumer L.float

pMountainTypes :: Parser MountainTypes
pMountainTypes = choice [
    try $ pSimpleLiteral "Thing" Things,
    try $ pSimpleLiteral "Bool" Bools,
    try $ pSimpleLiteral "Char" Chars,
    try $ pSimpleLiteral "String" Strings,
    try $ pSimpleLiteral "Int" Ints,
    try $ pSimpleLiteral "Float" Floats,
    try $ pSimpleLiteral "Unit" UnitType,
    try $ pSimpleLiteral "Nothing" EmptyType
  ]

-- ######################
-- ### StructureData  ###
-- ######################

pStructureData :: (Show a) => Parser a ->Parser (Structure a)
pStructureData pa = choice [
    try $ pLiteral pa,
    pTuple pa,
    pContext pa,
    try $ pLet pa,
    pRef
  ]

pLiteral :: Parser a -> Parser (Structure a)
pLiteral pa = Literal <$> pa

pRef :: (Show a) => Parser (Structure a)
pRef = Ref <$> pId

pTuple :: (Show a) => Parser a ->Parser (Structure a)
pTuple pa = pWrapBetween "(" ")" (pStructure pa)

pEnvDecl :: (Show a) => Parser a ->Parser (M.Map Id (Structure a))
pEnvDecl pa = do
  id_ <- pId
  _ <- symbol "="
  struct <- pStructure pa
  return $ M.singleton id_ struct

pEnv :: (Show a) => Parser a ->Parser (Env (Structure a))
pEnv pa = M.unions <$> pWrapBetween "<" ">" (sepEndBy (pEnvDecl pa) (pWrapWS ";"))

pContext :: (Show a) => Parser a ->Parser (Structure a)
pContext pa = do
  env <- pEnv pa
  _ <- pWrapWS "=>"
  struct <- pStructure pa
  return $ Context env struct

pLet :: (Show a) => Parser a ->Parser (Structure a)
pLet pa = do
  id <- pId
  _ <- pWrapWS "="
  a <- pStructure pa
  _ <- pWrapWS ";"
  b <- pStructure pa
  _ <- optional $ pWrapWS ";"
  return $ Let id a b

-- ######################
-- ### Structure Ops  ###
-- ######################

pStructureOp :: (Show a) => Parser a -> Parser (Structure a)
pStructureOp pa = makeExprParser (pCall pa) [
    [binaryR (pWrapWS "->") Function]
  ]

pCall :: (Show a) => Parser a ->Parser (Structure a)
pCall pa = do
  struct1 <- pStructureData pa
  struct2 <- many $ try (optional spaceConsumer *> (pCallInfix pa <|> pCallNormal pa))
  return $ foldl' (\state f -> f state) struct1 struct2

pCallInfix :: (Show a) => Parser a -> Parser (Structure a -> Structure a)
pCallInfix pa = do
  struct <- between (symbol "`") (symbol "`") (pStructureData pa)
  return $ Call struct

pCallNormal :: (Show a) => Parser a -> Parser (Structure a -> Structure a)
pCallNormal pa = do
  struct <- try (pStructureData pa)
  return (`Call` struct)

-- ##################
-- ### Structures ###
-- ##################

pMountainTerm :: Parser (Structure MountainLiteral)
pMountainTerm = pStructure pMountainLiteral

pStructure :: (Show a) => Parser a -> Parser (Structure a)
pStructure = pStructureOp

pMountain :: Parser (TypedStructure MountainTypes MountainLiteral)
pMountain = pTypedStructure pMountainTypes pMountainLiteral

pTypedStructure :: (Show typ, Show lit) => Parser typ -> Parser lit -> Parser (TypedStructure typ lit)
pTypedStructure pTyp pLit = Term <$> pStructure pLit

-- ######################
-- ### Full Importer  ###
-- ######################

basePath :: FilePath
basePath = "/home/mpriam/git/mtpl_language/src/SimpleRepository/"

fileExt :: String
fileExt = ".mtn"

envList :: [(String, MountainTerm)]
envList =[]

defaultState :: MountainState
defaultState = MountainState {
    changed=False,
    options=Options{
      parser=parseFile,
      repository=basePath,
      file_ext=fileExt},
    env=[M.fromList envList]
  }

runMountain :: MountainContextT IO MountainTerm -> IO (Either MountainError (MountainTerm, MountainState), [MountainLog])
runMountain = runMountainContextT defaultState

prettyLiteral :: MountainLiteral -> String
prettyLiteral (Thing id) = "#" ++ id
prettyLiteral (Bool b) = show b
prettyLiteral (Int i) = show i
prettyLiteral (Char c) = show c
prettyLiteral (String s) = show s
prettyLiteral (Float f) = show f
prettyLiteral x = show x

prettyMountainState :: MountainState -> String
prettyMountainState (MountainState _ e _) = show $ map prettyEnv e

prettyEnv :: Env MountainTerm -> String
prettyEnv xs = do
  let env_as_list = M.toList xs
  let terms = map (\(a,b) -> a ++ "=" ++ prettyMountain b) env_as_list
  "<" ++ intercalate ";" terms ++ ">"

prettyMountain :: MountainTerm -> String
prettyMountain (Literal a) = prettyLiteral a
prettyMountain (Ref id) = id
prettyMountain (Hole id) = "?" ++ id
prettyMountain (Function x y) = prettyMountain x ++ "->" ++ prettyMountain y
prettyMountain (Call a b) = "(" ++ prettyMountain a ++ ")(" ++ prettyMountain b ++ ")"
prettyMountain (Let id a b) = "(" ++ id ++ "=" ++ prettyMountain a ++ ";" ++ prettyMountain b ++ ")"
prettyMountain (Context env a) = do
  let env_as_list = M.toList env
  let terms = map (\(a,b) -> show a ++ ":" ++ prettyMountain b) env_as_list
  "(<" ++ intercalate ";" terms ++ ">  => " ++ prettyMountain a ++ ")"

prettyTypedMountain :: MountainTypedTerm -> String
prettyTypedMountain (Typed a b) = "(" ++ show a ++ ")::(" ++ prettyMountain b ++ ")"
prettyTypedMountain (Term t) = prettyMountain t

prettyLog :: [MountainLog] -> String
prettyLog (Step structure env:xs) = do
  prettyMountain structure ++ "\n" ++ prettyLog xs
prettyLog [] = "END"