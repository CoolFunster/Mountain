module MountainParser where

import Mountain

import Text.Megaparsec
import Text.Megaparsec.Error
import Text.Megaparsec.Char ( space1, string )
import Text.Megaparsec.Debug (dbg)
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.ByteString as B
import qualified Data.Map as M

import Control.Monad.Combinators.Expr

import Data.Text ( Text, pack, unpack, unpack, replace)
import Data.Void
import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.Char ( isSpace )
import Data.List ( intercalate, foldl', sort, isPrefixOf )

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
  result <- parseFileWith (pInnerScope pMountainLiteral) fp
  case result of
    Left err_str -> error err_str
    Right term -> return $ normalize (Scope term)

parseStringWith :: Parser a -> [Char] -> Either String a
parseStringWith cat_parser input_str = _baseParse cat_parser "String" (pack input_str)

parseString :: [Char] -> Either String MountainTerm
parseString = parseStringWith (normalize . Scope <$> pInnerScope pMountainLiteral)

parseTextWith :: Parser a -> Text -> Either String a
parseTextWith cat_parser = _baseParse cat_parser "String"

parseText :: Text -> Either String MountainTerm
parseText = parseTextWith (pScope pMountainLiteral)

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
  "import",
  "True",
  "False",
  "Thing",
  "Bool",
  "Char",
  "String",
  "Int",
  "Float",
  "All"]

restrictedIdChars :: [Char]
restrictedIdChars = "?$*#.,=:;@`[]{}()<>|&']-\\%"

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
-- ### MountainLiterals ###
-- ########################

pMountainLiteral :: Parser MountainLiteral
pMountainLiteral = choice [
  pThing,
  pBool,
  pChar,
  pString,
  try pFloat,
  pInt,
  pBigType]

pThing :: Parser MountainLiteral
pThing = symbol "#" *> (Thing <$> pId)

pBigType :: Parser MountainLiteral
pBigType =
    try (symbol "All") *> return All

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

-- ######################
-- ### StructureData  ###
-- ######################

pStructureData :: (Show a) => Parser a -> Parser (Structure a)
pStructureData pa = choice [
    try $ pLiteral pa,
    pImport pa,
    pWildcard,
    pSet pa,
    pTuple pa,
    pScope pa,
    pUnique pa,
    pContext pa,
    pReference
  ]

pLiteral :: Parser a -> Parser (Structure a)
pLiteral pa = Literal <$> pa

pWildcard :: Parser (Structure a)
pWildcard = symbol "?" *> return Wildcard

pReference :: Parser (Structure a)
pReference = Reference <$> pId

pImport :: (Show a) => Parser a -> Parser (Structure a)
pImport pa = symbol "import" *> spaceConsumer *> (Import <$> pStructure pa)

pSet :: (Show a) => Parser a -> Parser (Structure a)
pSet pa = Set <$> pWrapBetween "{" "}" (sepEndBy (pStructure pa) (pWrapWS ","))

pTuple :: (Show a) => Parser a -> Parser (Structure a)
pTuple pa = Tuple <$> pWrapBetween "(" ")" (sepEndBy (pStructure pa) (pWrapWS ","))

pInnerScope :: (Show a) => Parser a -> Parser [Structure a]
pInnerScope pa = sepEndBy (pStructure pa) (pWrapWS ";")

pScope :: (Show a) => Parser a -> Parser (Structure a)
pScope pa = Scope <$> pWrapBetween "\\{" "}" (pInnerScope pa)

emptyHash :: Hash
emptyHash = Hash B.empty

pUnique :: (Show a) => Parser a -> Parser (Structure a)
pUnique pa = Unique Unset <$> (symbol "$" *> pStructure pa)

pEnvDecl :: (Show a) => Parser a -> Parser (M.Map Id (Structure a))
pEnvDecl pa = do
  id_ <- pId
  _ <- symbol ":"
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

-- ######################
-- ### Structure Ops  ###
-- ######################


pStructureOp :: (Show a) => Parser a -> Parser (Structure a)
pStructureOp pa = makeExprParser (pCall pa) [
    [binaryR (pWrapWS ":") Bind],
    [binaryR (pWrapWS "@") Has],
    [binaryR (pWrapWS "&") (\lhs rhs -> Each [lhs,rhs])],
    [binaryR (pWrapWS "|") (\lhs rhs -> Either [lhs,rhs])],
    [binaryR (pWrapWS "~") (\lhs rhs -> Except [lhs,rhs])],
    [binaryR (pWrapWS "->") (\lhs rhs -> Function [lhs,rhs])],
    [binaryL (pWrapWS "%") Refine],
    [binaryR (pWrapWS "=") Bind]
  ]

pCall :: (Show a) => Parser a -> Parser (Structure a)
pCall pa = do
  struct1 <- pSelectHide pa
  struct2 <- many $ try (optional spaceConsumer *> (pCallInfix pa <|> pCallNormal pa))
  return $ foldl' (\state f -> f state) struct1 struct2

pCallInfix :: (Show a) => Parser a -> Parser (Structure a -> Structure a)
pCallInfix pa = do
  struct <- between (symbol "`") (symbol "`") (pSelectHide pa)
  res <- getInput 
  return $ Call struct

pCallNormal :: (Show a) => Parser a -> Parser (Structure a -> Structure a)
pCallNormal pa = do
  struct <- try (pSelectHide pa)
  res <- getInput 
  return (`Call` struct)

pSelectHideExt :: (Show a) => Structure a -> Parser (Structure a)
pSelectHideExt base = do
  s <- symbol ".-" <|> symbol "."
  ids <- choice [
    (: []) <$> try pId,
    pWrapBetween "[" "]" (sepEndBy pId (pWrapWS ","))]
  let res =
        case s of
          ".-" -> Hide base ids
          "." -> Select base ids
          other -> error "404"
  followup <- optional $ pSelectHideExt res
  case followup of
    Nothing -> return res
    Just x -> return x

pSelectHide :: (Show a) => Parser a -> Parser (Structure a)
pSelectHide pa = do
  struct1 <- pStructureData pa
  res <- optional $ pSelectHideExt struct1
  case res of
    Nothing -> return struct1
    Just x -> return x



-- ##################
-- ### Structure  ###
-- ##################

pStructure :: (Show a) => Parser a -> Parser (Structure a)
pStructure = pStructureOp

-- ######################
-- ### Full Importer  ###
-- ######################

basePath :: FilePath
basePath = "/home/mpriam/git/mtpl_language/src/Repository/"

fileExt :: String
fileExt = ".mtn"

defaultEnv :: MountainEnv
defaultEnv = MountainEnv {
    options=Options{
      parser=parseFile,
      repository=basePath,
      file_ext=fileExt},
    environment=[]
  }

dotImportFile :: String -> MountainContextT IO MountainTerm
dotImportFile fp = do
  opt <- getOptions
  let Options importer bp ext = opt
  let file_path = bp ++ dotPathAsDir fp ++ ext
  lift $ importer file_path

runMountain :: MountainContextT IO MountainTerm -> IO (Either MountainError (MountainTerm, MountainEnv), [MountainLog])
runMountain = runMountainContextT defaultEnv


prettyLiteral :: MountainLiteral -> String
prettyLiteral (Thing id) = "#" ++ id
prettyLiteral All = "All"

prettyTerm :: (Show a) => Structure a -> String
prettyTerm ((Literal a)) = show a
prettyTerm Wildcard = "?"
prettyTerm ((Reference id)) = id
prettyTerm ((Import a)) = "import " ++ prettyTerm a
prettyTerm ((Set as)) = "{" ++ intercalate "," (map prettyTerm as) ++ "}"
prettyTerm ((Tuple as)) = "(" ++ intercalate "," (map prettyTerm as) ++ ")"
prettyTerm ((Function as)) = "(" ++ intercalate "->" (map prettyTerm as) ++ ")"
prettyTerm ((Either as)) = "(" ++ intercalate "|" (map prettyTerm as) ++ ")"
prettyTerm ((Each as)) = "(" ++ intercalate "&" (map prettyTerm as) ++ ")"
prettyTerm ((Except as)) = "(" ++ intercalate "~" (map prettyTerm as) ++ ")"
prettyTerm ((Scope as)) = "\\{" ++ intercalate ";" (map prettyTerm as) ++ "}"
prettyTerm ((Unique _ as)) = "$" ++ prettyTerm as
prettyTerm ((Call a b)) = "(" ++ prettyTerm a ++ " " ++ prettyTerm b ++ ")"
prettyTerm ((Has a b)) = "(" ++ prettyTerm a ++ "@" ++ prettyTerm b ++ ")"
prettyTerm ((Bind a b)) = "(" ++ prettyTerm a ++ ":" ++ prettyTerm b ++ ")"
prettyTerm ((Refine a b)) = "(" ++ prettyTerm a ++ " % " ++ prettyTerm b ++ ")"
prettyTerm ((Select a [id])) = prettyTerm a ++ "." ++ id
prettyTerm ((Select a ids)) = prettyTerm a ++ "." ++ "[" ++ intercalate "," ids ++ "]"
prettyTerm ((Hide a [id])) = prettyTerm a ++ ".-" ++ id
prettyTerm ((Hide a ids)) = prettyTerm a ++ ".-" ++ "[" ++ intercalate "," ids ++ "]"
prettyTerm ((Context env a)) = do
  let env_as_list = M.toList env
  let terms = map (\(a,b) -> show a ++ ":" ++ prettyTerm b) env_as_list
  "(<" ++ intercalate "," terms ++ ">  => " ++ prettyTerm a ++ ")"

prettyMountainEnv :: MountainEnv -> String
prettyMountainEnv (MountainEnv _ e) = show $ map prettyEnv e

prettyEnv :: (Show a) => Env (Structure a) -> String
prettyEnv xs = do
  let env_as_list = M.toList xs
  let terms = map (\(a,b) -> a ++ ":" ++ prettyTerm b) env_as_list
  "<" ++ intercalate "," terms ++ ">"

instance (Show a) => Show (Log a) where
  show (Step term env) = do
    let pterm = prettyTerm term
    let res = "Step [" ++  intercalate "," (map prettyEnv env) ++ "] => " ++ pterm
    res