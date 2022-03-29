module FrontEnds.Textual.V1.CategoryParser where

import CategoryData

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
                (L.skipBlockComment (pack "/%") (pack "%/")) -- (4)

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

{- This wraps a parser around a pair of characters -}
pWrapBetween :: [Char] -> [Char] -> Parser a -> Parser a
pWrapBetween open_char close_char = between (pStringBetweenWS open_char) (pStringBetweenWS close_char)
-- {- megaparsec helpers end -}

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
    makeExprParser pFunctionTerm [
        [
            binaryR (pStringBetweenWS "->") combineFunctions
        ]
    ]

pFunctionTerm :: Parser Category
pFunctionTerm = pImport <|> pDefinition <|> pMembership

pImport :: Parser Category
pImport = do
    import_str <- symbol $ pack "import"
    _ <- spaceConsumer
    import_cat <- pCategory
    return Import{category_uri=import_cat}

pDefinition :: Parser Category
pDefinition = do
    define_str <- symbol $ pack "define"
    _ <- spaceConsumer
    category <- pPlaceholder
    return Definition{def_category=category}

pMembership :: Parser Category
pMembership =
    makeExprParser pPlaceholder [
        [
            binaryN (pStringBetweenWS "::") (\x y -> Membership{big_category=x,small_category=y})
        ]
    ]

{- If not a placeholder, falls through to other categories -}
pPlaceholder :: Parser Category
pPlaceholder = do
    parsed_name <- optional (try pCategoryName)
    let name = fromMaybe Unnamed parsed_name
    let at = pack "@"
    let colon = pack ":"
    parsed_ph_type <- optional (try (symbol at <|> symbol colon))
    let ph_type = case parsed_ph_type of
            Nothing -> Nothing
            Just some_val ->
                if some_val == at
                    then Just Element
                    else Just CategoryData.Label
    category <- pCategoryTerm
    case ph_type of
        Nothing -> return category
        Just ph_t -> return $ Placeholder name ph_t category

{- -}
pCategoryTerm :: Parser Category
pCategoryTerm = do
    base <- pStandardCategory
    extension <- many pCategoryExtension
    let collector = (\e b -> case b of
                d@Access{} -> d{base=e}
                mc@FunctionCall{} -> mc{base=e}
                anything_else -> anything_else
            )
    return (foldl collector base extension)

pCategoryExtension :: Parser Category
pCategoryExtension = choice [
        try pMorphismCallExtension,
        try pDereferenceExtension
    ]

pMorphismCallExtension :: Parser Category
pMorphismCallExtension = do
    argument <- pWrapBetween "[" "]" pCategory
    return FunctionCall{base=valid,argument=argument}

pDereferenceExtension :: Parser Category
pDereferenceExtension = do
    _ <- symbol (pack ".")
    category_id <- choice [
        try pCategoryIdx,
        try pCategoryName]
    return Access{base=valid, access_id=category_id}

pStandardCategory :: Parser Category
pStandardCategory = choice [
    try pUniversal,
    try pThing,
    try pReference,
    try pFlexible,
    try pComposition,
    try pTuple,
    try pCase,
    try pUnion,
    try pRefinement]

pThing :: Parser Category
pThing = do
    _ <- symbol (pack "`")
    thing_name <- pCategoryName
    return Thing{name=thing_name}

pReference :: Parser Category
pReference = do
    _ <- symbol (pack "$")
    name <- pCategoryName
    return Reference{name=name}

pUniversal :: Parser Category
pUniversal = do
    _ <- symbol (pack "Any")
    return (Special Universal)

pFlexible :: Parser Category
pFlexible = do
    _ <- symbol (pack "(%)")
    return (Special Flexible)


{- Parses composite types that have a beg & end character wrapping them -}
pCompositeTemplate :: CompositeType -> ([Char], [Char]) -> Parser Category
pCompositeTemplate c_type (beg_sep, end_sep) = do
    inner_categories <- pWrapBetween beg_sep end_sep pCategoryInnerList
    return Composite{composite_type=c_type, inner_categories=inner_categories}

pCategoryInnerList :: Parser [Category]
pCategoryInnerList = sepBy pCategory (pStringBetweenWS ",")


pTuple :: Parser Category
pTuple = pCompositeTemplate Tuple ("(", ")")

pUnion :: Parser Category
pUnion = pCompositeTemplate Union ("|", "|")

pComposition :: Parser Category
pComposition = pCompositeTemplate Composition ("*(", ")*")

pCase :: Parser Category
pCase = pCompositeTemplate Case ("*|", "|*")

pRefinementInner :: Parser Category
pRefinementInner = do
    ph <- pPlaceholder
    _ <- spaceConsumer
    _ <- symbol (pack "|")
    _ <- spaceConsumer
    refinement <- pCategory
    return Refined{base=ph,predicate=refinement}

pRefinement :: Parser Category
pRefinement = pWrapBetween "{" "}" pRefinementInner

-- Pretty Interpreter

prettyBasePath ::FilePath
prettyBasePath = "/home/mpriam/git/mtpl_language/src/FrontEnds/Textual/V1/Categories/"

loadPrettyModulePath :: FilePath -> ErrorableT IO Category
loadPrettyModulePath fp =
    let
        repl '.' = '/'
        repl c = c
        file_name = prettyBasePath ++ map repl fp
    in
        ErrorableT $ do
            file_exist <- doesFileExist (file_name ++ ".mtpl")
            dir_exist <- doesDirectoryExist file_name
            if file_exist
                then do
                    result <- parseCategoryFile (file_name ++ ".mtpl")
                    return (Valid result)
            else if dir_exist
                then do
                    dirContents <- listDirectory file_name
                    let baseDirContents = sort $ map takeBaseName dirContents
                    let loadedDir = map ((fp ++ ".") ++) baseDirContents
                    let zipped_dir = zip baseDirContents loadedDir
                    trace (show baseDirContents) $ return $ Valid $ Composite{composite_type=Tuple, inner_categories=
                        map (\(ref, path) -> Placeholder (Name ref) CategoryData.Label ((Import . Reference . Name) path)) zipped_dir
                    }
            else return $ ErrorList [Error BadImport [Reference (Name fp)]]

evaluatePrettyImport :: Category -> ErrorableT IO Category
evaluatePrettyImport (Import i@(Import _)) = evaluatePrettyImport i
evaluatePrettyImport (Import (Reference (Name n))) = loadPrettyModulePath n
evaluatePrettyImport (Import a@Access{base=bc, access_id=id}) = 
    let
        extractFilePath :: Category -> FilePath
        extractFilePath (Reference (Name n)) = n
        extractFilePath (Access bc (Name n)) = extractFilePath bc ++ "." ++ n
        extractFilePath _ = error "Something weird"
    in  
        loadPrettyModulePath (extractFilePath a)
evaluatePrettyImport (Import c@(Composite Tuple inner)) = do
    result <- mapM (evaluatePrettyImport . Import) inner
    return c{inner_categories=result}
evaluatePrettyImport (Import p@Placeholder{name=n, placeholder_type=CategoryData.Label, placeholder_category=pc}) = do
    result <- evaluatePrettyImport (Import pc)
    case result of
        p@Placeholder{placeholder_type=label, placeholder_category=ph_c} -> return $ p{name=n}
        _ -> return $ p{placeholder_category=result}
evaluatePrettyImport something = error $ "what is this import case? " ++ categoryToStr something

evaluatePrettyInner :: Category -> ErrorableT IO Category
evaluatePrettyInner i@Import{} = evaluatePrettyImport i >>= evaluatePrettyInner
evaluatePrettyInner other = evaluateInner other

evaluatePretty :: Category -> ErrorableT IO Category
evaluatePretty c = do
    cat <- postManipulateASTIO evaluatePrettyInner c
    if cat == c then return c else evaluatePretty cat

executePretty :: Category -> ErrorableT IO Category
executePretty input_cat = do
    let step1 = validateCategory input_cat >>= simplify
    case step1 of
        ErrorList ers -> ErrorableT $ return $ ErrorList ers
        Valid cat -> evaluatePretty cat