module Mountain.MountainParser where

import Mountain.Mountain

import Text.Megaparsec
import Text.Megaparsec.Error
import Text.Megaparsec.Char
import Text.Megaparsec.Debug (dbg)
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.ByteString as B
import qualified Data.Map as M

import Control.Monad.Combinators.Expr

import Data.Text ( Text, pack, unpack, unpack, replace)
import Data.Void
import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.Char ( isSpace )
import Data.List ( intercalate )

import Debug.Trace (trace)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.FilePath.Posix

import qualified Data.Text.IO as TextIO
import Data.List (sort)
import qualified Data.Set as Set
import Control.Monad.Trans
import Mountain.Hash


_baseParse :: Parser a -> FilePath -> Text -> Either String a
_baseParse parser file_path file_contents = do
    let result = runParser parser file_path file_contents
    case result of
        Left parse_error -> Left (errorBundlePretty parse_error)
        Right result -> Right result

parseFileWith :: Parser a -> FilePath -> IO (Either String a)
parseFileWith parser path_to_file = do
    contents <- TextIO.readFile path_to_file
    let category = _baseParse parser path_to_file contents
    return category

parseStringWith :: Parser a -> [Char] -> Either String a
parseStringWith cat_parser input_str = _baseParse cat_parser "String" (pack input_str)

parseString :: [Char] -> Either String MountainTerm
parseString = parseStringWith (pScope pMountainLiteral)

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

integer :: Parser Integer
integer = lexeme L.decimal

pWrapWS :: [Char] -> Parser String
pWrapWS input_str = try $ between (optional spaceConsumer) (optional spaceConsumer) (symbol input_str)

{- This wraps a parser around a pair of characters -}
pWrapBetween :: [Char] -> [Char] -> Parser a -> Parser a
pWrapBetween open_char close_char = between (symbol open_char <* optional spaceConsumer) (optional spaceConsumer *> symbol close_char)
-- {- megaparsec helpers end -}The func

reservedIds :: [String]
reservedIds = ["All", "function", "return", "import"]

restrictedIdChars :: [Char]
restrictedIdChars = "?$*#.,=:;@`[]{}()<>|&']-\\%"

pId :: Parser Id
pId = do
    name <- some $ satisfy isValidChar
    if name `elem` reservedIds
      then fail $ "Cannot use " ++ name ++ " as it is a reserved keyword"
      else return name
    where
      isValidChar :: Char -> Bool
      isValidChar c = (not $ c `elem` restrictedIdChars) && (not $ isSpace c)

-- ########################
-- ### MountainLiterals ###
-- ########################

pMountainLiteral :: Parser MountainLiteral
pMountainLiteral = pThing <|> pAll <|> pWildcard <|> pStar

pThing :: Parser MountainLiteral
pThing = symbol "#" *> (Thing <$> pId)

pAll :: Parser MountainLiteral
pAll = try (symbol "All") *> return All

pWildcard :: Parser MountainLiteral
pWildcard = (symbol "?") *> return Wildcard

pStar :: Parser MountainLiteral
pStar = symbol "*" *> return Star

-- ######################
-- ### StructureData  ###
-- ######################

pStructureData :: Parser a -> Parser (Structure a)
pStructureData pa = choice [
    pImport pa,
    pLiteral pa,
    pSet pa,
    pTuple pa,
    pScope pa,
    pUnique pa,
    pContext pa,
    pReference
  ]

pLiteral :: Parser a -> Parser (Structure a)
pLiteral pa = Literal <$> pa

pReference :: Parser (Structure a)
pReference = Reference <$> pId

pImport :: Parser a -> Parser (Structure a)
pImport pa = symbol "import" *> spaceConsumer *> (Import <$> pStructure pa)

pSet :: Parser a -> Parser (Structure a)
pSet pa = Set <$> pWrapBetween "{" "}" (sepEndBy (pStructure pa) (pWrapWS ","))

pTuple :: Parser a -> Parser (Structure a)
pTuple pa = Tuple <$> pWrapBetween "(" ")" (sepEndBy (pStructure pa) (pWrapWS ","))

pInnerScope :: Parser a -> Parser [Structure a]
pInnerScope pa = sepEndBy (pStructure pa) (pWrapWS ";")

pScope :: Parser a -> Parser (Structure a)
pScope pa = Scope <$> pWrapBetween "\\{" "}" (pInnerScope pa)

emptyHash :: Hash
emptyHash = Hash B.empty

pUnique :: Parser a -> Parser (Structure a)
pUnique pa = Unique (Hash B.empty) <$> (symbol "$" *> pStructure pa)

pEnvDecl :: Parser a -> Parser (M.Map Id (Structure a))
pEnvDecl pa = do
  id_ <- pId
  _ <- symbol ":"
  struct <- pStructure pa
  return $ M.singleton id_ struct

pEnv :: Parser a -> Parser (Env (Structure a))
pEnv pa = M.unions <$> pWrapBetween "<" ">" (sepEndBy (pEnvDecl pa) (pWrapWS ";"))

pContext :: Parser a -> Parser (Structure a)
pContext pa = do
  env <- pEnv pa
  _ <- pWrapWS "=>"
  struct <- pStructure pa
  return $ Context env struct

-- ######################
-- ### Structure Ops  ###
-- ######################


pStructureOp :: Parser a -> Parser (Structure a)
pStructureOp pa = makeExprParser (pCall pa) [
    [binaryR (pWrapWS ":") Bind],
    [binaryR (pWrapWS "@") Has],
    [binaryR (pWrapWS "&") (\lhs rhs -> Each [lhs,rhs])],
    [binaryR (pWrapWS "|") (\lhs rhs -> Either [lhs,rhs])],
    [binaryR (pWrapWS "->") (\lhs rhs -> Function [lhs,rhs])],
    [binaryL (pWrapWS "%") Refine],
    [binaryR (pWrapWS "=") Bind]
  ]

pCall :: Parser a -> Parser (Structure a)
pCall pa = do
  struct1 <- pSelect pa
  struct2 <- optional $ try (spaceConsumer *> pSelect pa)
  case struct2 of
    Nothing -> return struct1
    Just s2 -> return $ Call struct1 s2

pSelectExt :: Parser [Id]
pSelectExt = do
  s <- symbol "."
  choice [
    (: []) <$> try pId,
    pWrapBetween "[" "]" (sepEndBy pId (pWrapWS ","))]

pSelect :: Parser a -> Parser (Structure a)
pSelect pa = do
  struct1 <- pStructureData pa
  s <- many pSelectExt
  case concat s of
    [] -> return struct1
    res -> return $ Select struct1 res



-- ##################
-- ### Structure  ###
-- ##################

pStructure :: Parser a -> Parser (Structure a)
pStructure = pStructureOp

-- ######################
-- ### Full Importer  ###
-- ######################

basePath :: FilePath
basePath = "/home/mpriam/git/mtpl_language/src/Mountain/Repository/"

fileExt :: String
fileExt = ".mtn"

pFile :: FilePath -> IO MountainTerm
pFile fp = do
  result <- parseFileWith (pInnerScope pMountainLiteral) fp
  case result of
    Left err_str -> error err_str
    Right term -> return (Scope term)


importer :: MountainImporter
importer dot_import = do
  parsed_terms <- dotImportFile basePath fileExt (lift . pFile) dot_import
  let norm_terms = normalize parsed_terms
  return norm_terms

defaultEnv :: MountainEnv
defaultEnv = MountainEnv (importer, [])

runMountain :: MountainContextT IO MountainTerm -> IO (Either MountainError (MountainTerm, MountainEnv), [MountainLog])
runMountain = runMountainContextT defaultEnv

newtype PrettyStructure a = PrettyStructure (Structure a)
newtype PrettyMountainLiteral = PrettyLiteral MountainLiteral

type PrettyMountainTerm = PrettyStructure PrettyMountainLiteral

toPretty :: MountainTerm -> PrettyMountainTerm
toPretty t = PrettyStructure $ fmap PrettyLiteral t

fromPretty :: PrettyMountainTerm -> MountainTerm
fromPretty (PrettyStructure t) = fmap (\(PrettyLiteral a) -> a) t

instance Show PrettyMountainLiteral where
  show (PrettyLiteral (Thing id)) = "#" ++ show id
  show (PrettyLiteral All) = "All"
  show (PrettyLiteral Wildcard) = "?"
  show (PrettyLiteral Star) = "*"

instance (Show a) => Show (PrettyStructure a) where
  show (PrettyStructure (Literal a)) = show a
  show (PrettyStructure (Reference id)) = show id
  show (PrettyStructure (Import a)) = "import " ++ show a
  show (PrettyStructure (Set as)) = "{" ++ intercalate "," (map show as) ++ "}"
  show (PrettyStructure (Tuple as)) = "(" ++ intercalate "," (map show as) ++ ")"
  show (PrettyStructure (Function as)) = "(" ++ intercalate "->" (map show as) ++ ")"
  show (PrettyStructure (Either as)) = "(" ++ intercalate "|" (map show as) ++ ")"
  show (PrettyStructure (Each as)) = "(" ++ intercalate "&" (map show as) ++ ")"
  show (PrettyStructure (Scope as)) = "\\{" ++ intercalate ";" (map show as) ++ "}"
  show (PrettyStructure (Unique _ as)) = "$" ++ show as
  show (PrettyStructure (Call a b)) = "(" ++ show a ++ " " ++ show b ++ ")"
  show (PrettyStructure (Has a b)) = "(" ++ show a ++ "@" ++ show b ++ ")"
  show (PrettyStructure (Bind a b)) = "(" ++ show a ++ ":" ++ show b ++ ")"
  show (PrettyStructure (Refine a b)) = "(" ++ show a ++ " % " ++ show b ++ ")"
  show (PrettyStructure (Select a [id])) = show a ++ "." ++ id
  show (PrettyStructure (Select a ids)) = show a ++ "." ++ "[" ++ intercalate "," ids ++ "]"
  show (PrettyStructure (Context env a)) = do
    let env_as_list = M.toList env
    let terms = map (\(a,b) -> show a ++ ":" ++ show b) env_as_list
    "(<" ++ intercalate "," terms ++ ">  => " ++ show a ++ ")"