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

parseFile :: FilePath -> IO MountainTerm
parseFile fp = do
  result <- parseFileWith (pStructure pMountainExtern) fp
  case result of
    Left err_str -> error err_str
    Right term -> return term

parseStringWith :: Parser a -> [Char] -> Either String a
parseStringWith cat_parser input_str = _baseParse cat_parser "String" (pack input_str)

parseString :: [Char] -> Either String MountainTerm
parseString = parseStringWith (pStructure pMountainExtern)

parseTextWith :: Parser a -> Text -> Either String a
parseTextWith cat_parser = _baseParse cat_parser "String"

parseText :: Text -> Either String MountainTerm
parseText = parseTextWith (pStructure pMountainExtern)

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
  "assertFail"
  -- "Char",
  -- "String",
  -- "Int",
  -- "Float"
  ]

restrictedIdChars :: [Char]
-- restrictedIdChars = "!~?$*#.,=:;@`[]{}()<>|&']-\\%"
restrictedIdChars = "-=;`[]{}()<>#\""

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
-- ### MountainExterns ###
-- ########################

pMountainExtern :: Parser MountainExtern
pMountainExtern = choice [
  pThing,
  pBool,
  pChar,
  pString,
  try pFloat,
  pInt,
  try $ pSimpleExtern "assertIs" AssertIs,
  try $ pSimpleExtern "assertFail" AssertFail,
  try $ pSimpleExtern "assert" Assert,
  try $ pSimpleExtern "is" Is
  ]

pThing :: Parser MountainExtern
pThing = symbol "#" *> (Thing <$> pId)

pChar :: Parser MountainExtern
pChar = Char <$> between (symbol "\'") (symbol "\'") (try escaped <|> normalChar)
   where
     escaped :: Parser Char
     escaped = do {
        _ <- symbol "\\"
      ; satisfy (=='\'')
     }
     normalChar :: Parser Char
     normalChar = satisfy (/= '\'')

pString :: Parser MountainExtern
pString = String <$> between (symbol "\"") (symbol "\"") (many (try escaped <|> normalChar))
   where
     escaped :: Parser Char
     escaped = do {
        _ <- symbol "\\"
      ; satisfy (=='\"')
     }
     normalChar :: Parser Char
     normalChar = satisfy (/= '\"')

pBool :: Parser MountainExtern
pBool = Bool <$> do
  res <- symbol "True" <|> symbol "False"
  case res of
    "True" -> return True
    "False" -> return False
    _ -> error "unreachable"

pInt :: Parser MountainExtern
pInt = Int <$> L.signed (L.space Text.Megaparsec.empty Text.Megaparsec.empty Text.Megaparsec.empty) L.decimal

pFloat :: Parser MountainExtern
pFloat = Float <$> L.signed spaceConsumer L.float

pSimpleExtern :: String -> MountainExtern -> Parser MountainExtern
pSimpleExtern s l = symbol s $> l

-- ######################
-- ### StructureData  ###
-- ######################

pStructureData :: (Show a) => Parser a -> Parser (Structure a)
pStructureData pa = choice [
    try $ pExtern pa,
    pTuple pa,
    pContext pa,
    try $ pLet pa,
    pVar
  ]

pExtern :: Parser a -> Parser (Structure a)
pExtern pa = Extern <$> pa

-- pWildcard :: Parser (Structure a)
-- pWildcard = symbol "?" Data.Functor.$> Wildcard

pVar :: Parser (Structure a)
pVar = Var <$> pId

pTuple :: (Show a) => Parser a -> Parser (Structure a)
pTuple pa = pWrapBetween "(" ")" (pStructure pa)

pEnvDecl :: (Show a) => Parser a -> Parser (M.Map Id (Structure a))
pEnvDecl pa = do
  id_ <- pId
  _ <- symbol "="
  struct <- pStructure pa
  return $ M.singleton id_ struct

pEnv :: (Show a) => Parser a -> Parser (Env (Structure a))
pEnv pa = M.unions <$> pWrapBetween "<" ">" (sepEndBy (pEnvDecl pa) (pWrapWS ";"))

pContext :: (Show a) => Parser a -> Parser (Structure a)
pContext pa = do
  env <- pEnv pa
  _ <- pWrapWS "=>"
  struct <- pStructure pa
  return $ Context env struct

pLet :: (Show a) => Parser a -> Parser (Structure a)
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

colonOp :: Parser String
colonOp = (lexeme . try) (between (optional spaceConsumer) (optional spaceConsumer) $ symbol ":" <* notFollowedBy (symbol ":"))

pStructureOp :: (Show a) => Parser a -> Parser (Structure a)
pStructureOp pa = makeExprParser (pCall pa) [
    [binaryR (pWrapWS "->") Function]
  ]

pCall :: (Show a) => Parser a -> Parser (Structure a)
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
-- ### Structure  ###
-- ##################

pStructure :: (Show a) => Parser a -> Parser (Structure a)
pStructure = pStructureOp

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

-- dotImportFile :: String -> MountainContextT IO MountainTerm
-- dotImportFile fp = do
--   opt <- getOptions
--   let Options importer bp ext = opt
--   let file_path = bp ++ dotPathAsDir fp ++ ext
--   lift $ importer file_path

runMountain :: MountainContextT IO MountainTerm -> IO (Either MountainError (MountainTerm, MountainState), [MountainLog])
runMountain = runMountainContextT defaultState

prettyExtern :: MountainExtern -> String
prettyExtern (Thing id) = "#" ++ id
prettyExtern (Bool b) = show b
prettyExtern (Int i) = show i
prettyExtern (Char c) = show c
prettyExtern (String s) = show s
prettyExtern (Float f) = show f
prettyExtern x = show x

prettyMountainState :: MountainState -> String
prettyMountainState (MountainState _ e _) = show $ map prettyEnv e

prettyEnv :: Env MountainTerm -> String
prettyEnv xs = do
  let env_as_list = M.toList xs
  let terms = map (\(a,b) -> a ++ "=" ++ prettyMountain b) env_as_list
  "<" ++ intercalate ";" terms ++ ">"

prettyMountain :: MountainTerm -> String
prettyMountain (Extern a) = prettyExtern a
prettyMountain (Var id) = id
prettyMountain (Function x y) = prettyMountain x ++ "->" ++ prettyMountain y
prettyMountain (Call a b) = "(" ++ prettyMountain a ++ ")(" ++ prettyMountain b ++ ")"
prettyMountain (Let id a b) = "(" ++ id ++ "=" ++ prettyMountain a ++ ";" ++ prettyMountain b ++ ")"
prettyMountain (Context env a) = do
  let env_as_list = M.toList env
  let terms = map (\(a,b) -> show a ++ ":" ++ prettyMountain b) env_as_list
  "(<" ++ intercalate ";" terms ++ ">  => " ++ prettyMountain a ++ ")"

prettyLog :: [MountainLog] -> String
prettyLog (Step structure env:xs) = do
  prettyMountain structure ++ "\n" ++ prettyLog xs
prettyLog [] = "END"