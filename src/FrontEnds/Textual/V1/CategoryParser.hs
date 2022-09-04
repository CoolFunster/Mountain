module FrontEnds.Textual.V1.CategoryParser where

import Category
import FrontEnds.Textual.V1.CategoryWriter

import Text.Megaparsec
import Text.Megaparsec.Error
import Text.Megaparsec.Char
import Text.Megaparsec.Debug (dbg)
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad.Combinators.Expr

import Data.Text ( Text, pack, unpack, unpack, replace)
import Data.Char
import Data.Void
import Data.Maybe (fromJust, isJust, fromMaybe)

import Debug.Trace (trace)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.FilePath.Posix

import qualified Data.Text.IO as TextIO
import Data.List (sort)
import qualified Data.Set as Set
import Control.Monad.Trans


_baseParseCategory :: Parser a -> FilePath -> Text -> Either String a
_baseParseCategory cat_parser file_path file_contents = do
    let result = runParser cat_parser file_path file_contents
    case result of
        Left parse_error -> Left (errorBundlePretty parse_error)
        Right result -> Right result

parseCategoryFileWith :: Parser a -> FilePath -> IO (Either String a)
parseCategoryFileWith parser path_to_file = do
    contents <- TextIO.readFile path_to_file
    let category = _baseParseCategory parser path_to_file contents
    return category

parseCategoryFile :: FilePath -> IO (Either String Category)
parseCategoryFile = parseCategoryFileWith pCategory

parseCategoryStringWith :: Parser a -> [Char] -> Either String a
parseCategoryStringWith cat_parser input_str = _baseParseCategory cat_parser "String" (pack input_str)

parseCategoryString :: [Char] -> Either String Category
parseCategoryString = parseCategoryStringWith pCategory

parseCategoryTextWith :: Parser a -> Text -> Either String a
parseCategoryTextWith cat_parser = _baseParseCategory cat_parser "String"

parseCategoryText :: Text -> Either String Category
parseCategoryText = parseCategoryTextWith pCategory

type Parser = Parsec Void Text

{- megaparsec helpers -}
spaceConsumer :: Parser ()
spaceConsumer = L.space
                space1                         -- (2)
                (L.skipLineComment $ pack "//")       -- (3)
                (L.skipBlockComment (pack "/*") (pack "*/")) -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

binaryL  inner_parser f = InfixL  (f <$ inner_parser)
binaryR  inner_parser f = InfixR  (f <$ inner_parser)
binaryN  inner_parser f = InfixN  (f <$ inner_parser)
prefix  inner_parser f = Prefix  (f <$ inner_parser)
postfix inner_parser f = Postfix (f <$ inner_parser)

integer :: Parser Integer
integer = lexeme L.decimal

pStringBetweenWS :: [Char] -> Parser Text
pStringBetweenWS input_str = try $ between (optional spaceConsumer) (optional spaceConsumer) (symbol (pack input_str))

withValidation :: Parser Category -> Parser Category
withValidation parser = do
    offset <- getOffset
    -- result <- dbg "validation" parser
    result <- parser
    let validated_result = getResultOf $ validateCategory result
    case validated_result of
        Right cat -> return cat
        Left errors -> region (setErrorOffset offset) (fail (concatMap errorToString errors))

{- This wraps a parser around a pair of characters -}
pWrapBetween :: [Char] -> [Char] -> Parser a -> Parser a
pWrapBetween open_char close_char = between (symbol (pack open_char) <* optional spaceConsumer) (optional spaceConsumer *> symbol (pack close_char))
-- {- megaparsec helpers end -}The func

reserved :: [String]
reserved = ["Any", "function", "return", "import", ".", "#", "$", "_", "?"]

pCategoryName :: Parser Id
pCategoryName = do
    name <- some (alphaNumChar <|> char '_')
    case name of
        other ->
          if other `elem` reserved
            then fail $ "Cannot use " ++ other ++ " as it is a reserved keyword"
            else return $ Name other

pCategory :: Parser Category
pCategory = pPlaceholder

pPlaceholder :: Parser Category
pPlaceholder = withValidation $ makeExprParser (try pFunction) [
            [
                binaryR (pStringBetweenWS "@") (\Reference{name=n} rhs -> Placeholder n Variable rhs),
                binaryR (pStringBetweenWS ":") (\Reference{name=n} rhs -> Placeholder n Category.Label rhs)
            ]]

pFunction :: Parser Category
pFunction =
    let
        combineFunctions :: Category -> Category -> Category
        combineFunctions a c@Composite{composite_type=Function,inner_categories=inner} = c{inner_categories=a:inner}
        combineFunctions a b = Composite{composite_type=Function, inner_categories=[a,b]}
    in
      withValidation $ makeExprParser (try pFunctionTerm) [
          [binaryR (pStringBetweenWS "->") combineFunctions]]

pFunctionTerm :: Parser Category
pFunctionTerm = pImport <|> pFunctionOrReturn

pImport :: Parser Category
pImport = withValidation $ do
    import_str <- symbol $ pack "import"
    _ <- spaceConsumer
    import_cat <- pCategoryTerm
    return Import{import_category=import_cat}

pFunctionOrReturn :: Parser Category
pFunctionOrReturn = do
    function_or_return <- optional $ choice [
        try $ symbol (pack "function") <* spaceConsumer,
        try $ symbol (pack "return") <* spaceConsumer
      ]
    case function_or_return of
      Nothing -> try pCall
      Just _ -> pCategory

-- pCall :: Parser Category
-- pCall = withValidation $ makeExprParser (lookAhead pCategoryTerm) [
--           [binaryL (symbol (pack " ")) Call]]

pCall :: Parser Category
pCall = withValidation $ do
  base <- pTypeAnnotation
  pCallArg base

pCallArg :: Category -> Parser Category
pCallArg base = do
   i <- getInput
   call_term <- observing $ try $ spaceConsumer *> pTypeAnnotation
   i <- getInput
   case call_term of
     Left pe -> return base
     Right cat -> pCallArg (Call base cat)


pTypeAnnotation :: Parser Category
pTypeAnnotation = withValidation $ makeExprParser (try pCategoryTerm) [[
                binaryR (pStringBetweenWS "::") TypeAnnotation
            ]]

pCategoryTerm :: Parser Category
pCategoryTerm = try pAccess <|> try pStandardCategory

pAccess :: Parser Category
pAccess = withValidation $ do
  base <- pStandardCategory
  ids <- some pAccessExt
  return $ foldl (\c id -> Access{base=c, access_type=id}) base ids

pAccessExt :: Parser AccessType
pAccessExt = choice [
      try pDotAccess,
      try pBracketAccess
    ]

pDotAccess :: Parser AccessType
pDotAccess = do
  _ <- symbol (pack ".")
  cat_name <- try pCategoryName
  return $ ByLabelGroup [cat_name]

pBracketAccess :: Parser AccessType
pBracketAccess = do
  id_list <- pWrapBetween "[" "]" (sepEndBy pCategoryName (pStringBetweenWS ","))
  return $ ByLabelGroup id_list

pStandardCategory :: Parser Category
pStandardCategory = choice [
    try pAny,
    try pVarAny,
    try pThing,
    try pReference,
    try pFlexible,
    try pSet,
    try pTuple,
    try pEither,
    try pComposition,
    try pMatch,
    try pRefinement,
    try pScope]

pThing :: Parser Category
pThing = withValidation $ do
    _ <- symbol (pack "#")
    thing_name <- pCategoryName
    return Thing{name=thing_name}

pSet :: Parser Category
pSet = withValidation $ do
  inner_categories <- pWrapBetween "{" "}" (pCategoryInnerList ",")
  return Set{elements=inner_categories}

pReference :: Parser Category
pReference = withValidation $ do
    _ <- optional $ symbol $ pack "$"
    name <- pCategoryName
    return Reference{name=name}

pVarAny :: Parser Category
pVarAny = withValidation $ do
  _ <- symbol (pack "_")
  return $ Placeholder Unnamed Variable universal

pAny :: Parser Category
pAny = withValidation $ do
    _ <- symbol (pack "Any")
    return (Special Any)

pFlexible :: Parser Category
pFlexible = withValidation $ do
    _ <- symbol (pack "?")
    id <- optional pCategoryName
    case id of
      Nothing -> return $ Placeholder Unnamed Variable (Special Flexible)
      Just id -> return $ Placeholder id Variable (Special Flexible)

{- Parses composite types that have a beg & end character wrapping them -}
pCompositeTemplate :: CompositeType -> ([Char], [Char], [Char]) -> Parser Category
pCompositeTemplate c_type (beg_sep, mid_sep, end_sep) = withValidation $ do
    inner_categories <- pWrapBetween beg_sep end_sep (pCategoryInnerList mid_sep)
    return Composite{composite_type=c_type, inner_categories=inner_categories}

pCategoryInnerList :: [Char] -> Parser [Category]
pCategoryInnerList mid_sep = sepEndBy pCategory (pStringBetweenWS mid_sep) <* optional (pStringBetweenWS mid_sep)


pTuple :: Parser Category
pTuple = pCompositeTemplate Tuple ("(", ",", ")")

pEither :: Parser Category
pEither = pCompositeTemplate Either ("*|", ",", "|*")

pComposition :: Parser Category
pComposition = pCompositeTemplate Composition ("(", ";", ")")

pMatch :: Parser Category
pMatch = pCompositeTemplate Match ("*|", ";", "|*")

pRefinementInner :: Parser Category
pRefinementInner = withValidation $ do
    ph <- pPlaceholder
    _ <- spaceConsumer
    _ <- symbol (pack "|")
    _ <- spaceConsumer
    refinement <- pCategory
    return Refined{base=ph,predicate=refinement}

pRefinement :: Parser Category
pRefinement = pWrapBetween "{" "}" pRefinementInner

pScope :: Parser Category
pScope = withValidation $ do
  let parseInnerScope = sepEndBy (try pBinding <|> try pCategory) (pStringBetweenWS ";")
  stuff <- pWrapBetween "*{" "}*" parseInnerScope
  return $ Scope stuff


pBinding :: Parser Category
pBinding = withValidation $ makeExprParser pCategory [
    [binaryR (pStringBetweenWS "=") (\lhs rhs ->
      case lhs of
        Reference n -> Binding (Placeholder n Variable universal) rhs
        _ -> Binding lhs rhs)
    ]
  ]

-- Pretty loader

textualBasePath :: FilePath
textualBasePath = "/home/mpriam/git/mtpl_language/src/FrontEnds/Textual/V1/Categories/"

textualFileExt :: String
textualFileExt = ".mtn"

pTextualFile :: FilePath -> CategoryContextT IO Category
pTextualFile fp = do
  result <- lift $ parseCategoryFileWith pCategory fp
  case result of
    Left err_str -> error err_str
    Right cat -> return cat

loadTextual :: FilePath -> CategoryContextT IO Category
loadTextual = loadModule textualBasePath textualFileExt pTextualFile
