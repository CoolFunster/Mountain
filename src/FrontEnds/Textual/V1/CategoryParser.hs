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


_baseParseCategory :: Parser a -> FilePath -> Text -> a
_baseParseCategory cat_parser file_path file_contents = do
    let result = runParser cat_parser file_path file_contents
    case result of
        Left parse_error -> error (errorBundlePretty parse_error)
        Right result -> result

parseCategoryFileWith :: Parser a -> FilePath -> IO a
parseCategoryFileWith parser path_to_file = do
    contents <- TextIO.readFile path_to_file
    let category = _baseParseCategory parser path_to_file contents
    return category

parseCategoryFile :: FilePath -> IO Category
parseCategoryFile = parseCategoryFileWith pCategory

parseCategoryStringWith :: Parser a -> [Char] -> a
parseCategoryStringWith cat_parser input_str = _baseParseCategory cat_parser "String" (pack input_str)

parseCategoryString :: [Char] -> Category
parseCategoryString = parseCategoryStringWith pCategory

parseCategoryTextWith :: Parser a -> Text -> a
parseCategoryTextWith cat_parser = _baseParseCategory cat_parser "String"

parseCategoryText :: Text -> Category
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
pWrapBetween open_char close_char = between (pStringBetweenWS open_char) (pStringBetweenWS close_char)
-- {- megaparsec helpers end -}The func

pCategoryName :: Parser Id
pCategoryName = do
    name <- some (alphaNumChar <|> char '_')
    case name of
        "Any" -> fail "Cannot name category Any"
        _ -> return (Name name)

pCategoryIdx :: Parser Id
pCategoryIdx = do
    _ <- symbol $ pack "#"
    Index . fromInteger <$> integer

pCategory = pFunction

pFunction :: Parser Category
pFunction =
    let
        combineFunctions :: Category -> Category -> Category
        combineFunctions a c@Composite{composite_type=Function,inner_categories=inner} = c{inner_categories=a:inner}
        combineFunctions a b = Composite{composite_type=Function, inner_categories=[a,b]}
    in
        withValidation $ makeExprParser pFunctionTerm [
            [binaryR (pStringBetweenWS "->") combineFunctions]]

pFunctionTerm :: Parser Category
pFunctionTerm = do
    _ <- optional spaceConsumer
    try pImport <|> pGivenOrReturn
  
pImport :: Parser Category
pImport = withValidation $ do
    import_str <- symbol $ pack "import"
    _ <- spaceConsumer
    import_cat <- pCategory
    return Import{import_category=import_cat}

pGivenOrReturn :: Parser Category
pGivenOrReturn = do
    given_or_return <- optional (symbol (pack "given") <|> symbol (pack "return"))
    _ <- optional spaceConsumer
    pTypeAnnotation

pTypeAnnotation :: Parser Category
pTypeAnnotation = withValidation $ makeExprParser pPlaceholder [
            [
                binaryN (pStringBetweenWS "::") (\x y -> TypeAnnotation{big_category=x,small_category=y})
            ]]

{- If not a placeholder, falls through to other categories -}
pPlaceholder :: Parser Category
pPlaceholder = withValidation $ do
    parsed_name <- optional (try pCategoryName)
    let name = fromMaybe Unnamed parsed_name
    let at = pack "@"
    let colon = pack ":"
    parsed_ph_type <- optional (try (symbol at <|> symbol colon))
    let ph_type = case parsed_ph_type of
            Nothing -> Nothing
            Just some_val ->
                if some_val == at
                    then Just Variable
                    else Just Category.Label
    category <- pCategoryTerm
    case ph_type of
        Nothing -> return category
        Just ph_t -> return $ Placeholder name ph_t category

pCategoryTerm :: Parser Category
pCategoryTerm = withValidation $ do
    base <- pStandardCategory
    extension <- many pCategoryExtension
    let collector = (\e b -> case b of
                d@Access{} -> d{base=e}
                mc@Call{} -> mc{base=e}
                anything_else -> anything_else
            )
    return (foldl collector base extension)

pCategoryExtension :: Parser Category
pCategoryExtension = choice [
        try pCallExtension,
        try pAccessExtension
    ]

pCallExtension :: Parser Category
pCallExtension = do
    argument <- pWrapBetween "[" "]" pCategory
    return Call{base=valid,argument=argument}

pAccessExtension :: Parser Category
pAccessExtension = do
    _ <- symbol (pack ".")
    category_id <- choice [
        try pCategoryIdx,
        try pCategoryName]
    return Access{base=valid, access_id=category_id}

pStandardCategory :: Parser Category
pStandardCategory = choice [
    try pAny,
    try pThing,
    try pReference,
    try pFlexible,
    try pComposition,
    try pSet,
    try pTuple,
    try pMatch,
    try pEither,
    try pRefinement]

pThing :: Parser Category
pThing = withValidation $ do
    _ <- symbol (pack "`")
    thing_name <- pCategoryName
    return Thing{name=thing_name}

pSet :: Parser Category 
pSet = withValidation $ do
  inner_categories <- pWrapBetween "{" "}" pCategoryInnerList
  return Set{elements=inner_categories}

pReference :: Parser Category
pReference = withValidation $ do
    _ <- symbol (pack "$")
    name <- pCategoryName
    return Reference{name=name}

pAny :: Parser Category
pAny = withValidation $ do
    _ <- symbol (pack "Any")
    return (Special Any)

pFlexible :: Parser Category
pFlexible = withValidation $ do
    _ <- symbol (pack "(%)")
    return (Special Flexible)


{- Parses composite types that have a beg & end character wrapping them -}
pCompositeTemplate :: CompositeType -> ([Char], [Char]) -> Parser Category
pCompositeTemplate c_type (beg_sep, end_sep) = withValidation $ do
    inner_categories <- pWrapBetween beg_sep end_sep pCategoryInnerList
    return Composite{composite_type=c_type, inner_categories=inner_categories}

pCategoryInnerList :: Parser [Category]
pCategoryInnerList = sepBy pCategory (pStringBetweenWS ",")


pTuple :: Parser Category
pTuple = pCompositeTemplate Tuple ("(", ")")

pEither :: Parser Category
pEither = pCompositeTemplate Either ("|", "|")

pComposition :: Parser Category
pComposition = pCompositeTemplate Composition ("*(", ")*")

pMatch :: Parser Category
pMatch = pCompositeTemplate Match ("*|", "|*")

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

-- Pretty loader

textualBasePath :: FilePath
textualBasePath = "/home/mpriam/git/mtpl_language/src/FrontEnds/Textual/V1/Categories/"

textualFileExt :: String
textualFileExt = ".mtpl"

pTextualFile :: FilePath -> CategoryContextT IO Category
pTextualFile fp = lift $ parseCategoryFileWith pCategory fp

loadTextual :: FilePath -> CategoryContextT IO Category
loadTextual = loadModule textualBasePath textualFileExt pTextualFile
