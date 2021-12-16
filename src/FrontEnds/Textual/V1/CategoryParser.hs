module FrontEnds.Textual.V1.CategoryParser where

import CategoryData

import Text.Megaparsec
import Control.Monad.Combinators.Expr
import Text.Megaparsec.Error
import Text.Megaparsec.Char
import Text.Megaparsec.Debug (dbg)
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Text ( Text, pack, unpack, pack, unpack )
import Data.Void
import Data.Maybe (fromJust, isJust)

import Debug.Trace (trace)

import qualified Data.Text.IO as TextIO


baseParseCategory :: Parser a -> FilePath -> Text -> a
baseParseCategory cat_parser file_path file_contents = do
    let result = runParser cat_parser file_path file_contents
    case result of
        Left parse_error -> error (errorBundlePretty parse_error)
        Right result -> result

parseCategoryFileWith :: Parser a -> FilePath -> IO a
parseCategoryFileWith parser path_to_file = do
    contents <- TextIO.readFile path_to_file
    let category = baseParseCategory parser path_to_file contents
    return category

parseCategoryFile :: FilePath -> IO Category
parseCategoryFile = parseCategoryFileWith pCategory

parseCategoryStringWith :: Parser a -> [Char] -> a
parseCategoryStringWith cat_parser input_str = baseParseCategory cat_parser "String" (pack input_str)

parseCategoryString :: [Char] -> Category
parseCategoryString = parseCategoryStringWith pCategory

parseCategoryTextWith :: Parser a -> Text -> a
parseCategoryTextWith cat_parser = baseParseCategory cat_parser "String"

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
{- megaparsec helpers end -}

{- category helpers-}
pCategoryName :: Parser Id
pCategoryName = do
    name <- some (alphaNumChar <|> char '_')
    return (Name name)

pCategoryIdx :: Parser Id
pCategoryIdx = do
    _ <- symbol $ pack "#"
    Index . fromInteger <$> integer

pCategoryLabel :: Parser Id
pCategoryLabel = do
    name <- pCategoryName
    _ <- symbol (pack ":")
    if name `elem` [Name "given", Name "import", Name "define", Name "return"]
        then fail $ "cannot name category keyword " ++ show name
        else return name

pCategoryOptionalLabel :: Parser Id
pCategoryOptionalLabel = do
    result <- optional (try pCategoryLabel)
    case result of
        Just inner_val -> return inner_val
        Nothing -> return Unnamed

pCategoryInnerList :: Parser [Category]
pCategoryInnerList = sepBy pCategory (try $ optional spaceConsumer <* symbol (pack ","))
{- end category helpers-}

pCategory = pMembership

pMembership :: Parser Category
pMembership =
    let
        expr_parser = makeExprParser pOptionalLabeledCategory [
                [
                    binaryN (symbol (pack "::")) (\x y -> Membership{big_category=x,small_category=y})
                ]
            ]
    in
        do
            expr_parser

pOptionalLabeledCategory :: Parser Category
pOptionalLabeledCategory = do
    name <- pCategoryOptionalLabel
    category <- try pLabeledCategory <|> pCategoryTerm
    case name of
        Unnamed -> return category
        _ -> return CategoryData.Label{name=name, target=category}

pLabeledCategory :: Parser Category
pLabeledCategory = do
    name <- pCategoryLabel
    category <- pCategoryTerm
    return CategoryData.Label{name=name, target=category}

pCategoryTerm :: Parser Category
pCategoryTerm = do
    base <- pCategoryBase
    extension <- many pCategoryExtension
    let collector = (\e b -> case b of
                d@Dereference{} -> d{base_category=e}
                mc@MorphismCall{} -> mc{base_morphism=e}
                anything_else -> anything_else
            )
    return (foldl collector base extension)

pCategoryExtension :: Parser Category
pCategoryExtension = choice [
        try pMorphismCallExtension,
        try pDereferenceExtension
    ]

pCategoryBase :: Parser Category
pCategoryBase = pStandardCategory <|> try pThing <|> try pReference <|> try pPlaceholder


pStandardCategory :: Parser Category
pStandardCategory = choice [
    try pMorphism,
    try pUniversal,
    try pFlexible,
    try pComposition,
    try pTuple,
    try pSumposition,
    try pSumple,
    try pHigher,
    try pRefinement]

pThing :: Parser Category
pThing = do
    _ <- symbol (pack "`")
    thing_name <- pCategoryName
    return Thing{name=thing_name}

pWrapInner :: [Char] -> [Char] -> Parser a -> Parser a
pWrapInner open_char close_char = between (optional spaceConsumer <* symbol (pack open_char) <* optional spaceConsumer) (optional spaceConsumer <* symbol (pack close_char) <* optional spaceConsumer)

pTuple :: Parser Category
pTuple = do
    inner_categories <- pWrapInner "(" ")" pCategoryInnerList
    if length inner_categories == 1
        then return $ head inner_categories
        else return Composite{composition_type=Product, inner=inner_categories}

pSumple :: Parser Category
pSumple = do
    inner_categories <- pWrapInner "|" "|" pCategoryInnerList
    if length inner_categories == 1
        then return $ head inner_categories
        else return Composite{composition_type=Sum, inner=inner_categories}

pHigher :: Parser Category
pHigher = do
    inner_categories <- pWrapInner "^{" "}" pCategoryInnerList
    if length inner_categories == 1
        then return $ head inner_categories
        else return Composite{composition_type=Higher, inner=inner_categories}

pComposition :: Parser Category
pComposition = do
    inner_categories <- pWrapInner "*(" ")" pCategoryInnerList
    if length inner_categories == 1
        then return $ head inner_categories
        else return Composite{composition_type=Composition, inner=inner_categories}

pSumposition :: Parser Category
pSumposition = do
    inner_categories <- pWrapInner "*|" "|" pCategoryInnerList
    if length inner_categories == 1
        then return $ head inner_categories
        else return Composite{composition_type=Sumposition, inner=inner_categories}

pCategoryLevel :: Parser CategoryLevel
pCategoryLevel = do
    specific_result <- try $ optional $ between (symbol $ pack "<") (symbol $ pack ">") integer
    case specific_result of
        Nothing -> return AnyLevel
        Just some_level -> return $ Specific some_level

pPlaceholder :: Parser Category
pPlaceholder = do
    name <- optional pCategoryName
    ph_level <- pCategoryLevel
    _ <- symbol (pack "@")
    category <- pCategory
    case name of
        Just some_name -> return Placeholder{name=some_name,ph_level=ph_level,ph_category=category}
        Nothing -> return Placeholder{name=Unnamed,ph_level=ph_level,ph_category=category}

pRefinementInner :: Parser Category
pRefinementInner = do
    ph <- pPlaceholder
    _ <- spaceConsumer
    _ <- symbol (pack "|")
    _ <- spaceConsumer
    refinement <- pCategory
    return RefinedCategory{base_category=ph,predicate=refinement}

pRefinement :: Parser Category
pRefinement = between (symbol (pack "{")) (symbol (pack "}")) pRefinementInner

pFlexible :: Parser Category
pFlexible = do
    _ <- symbol (pack "(%)")
    return (Special Flexible)

pUniversal :: Parser Category
pUniversal = do
    _ <- symbol (pack "Any")
    return (Special Universal)

pReference :: Parser Category
pReference = do
    _ <- symbol (pack "$")
    name <- pCategoryName
    return Reference{name=name}


pMorphismCallExtension :: Parser Category
pMorphismCallExtension = do
    argument <- between (symbol (pack "[")) (symbol (pack "]")) pCategory
    return MorphismCall{base_morphism=Thing Unnamed,argument=argument}

pDereferenceExtension :: Parser Category
pDereferenceExtension = do
    _ <- symbol (pack ".")
    category_id <- choice [
        try pCategoryIdx,
        try pCategoryName]
    return Dereference{base_category=Thing Unnamed, category_id=category_id}

pMorphismTermType :: Parser MorphismTermType
pMorphismTermType =
    do
        pString <- choice [
            symbol $ pack "import",
            symbol $ pack "given",
            symbol $ pack "define",
            symbol $ pack "return"]
        _ <- spaceConsumer
        case unpack pString of
            "import" -> return Import
            "given" -> return Given
            "define" -> return Definition
            "return" -> return Return
            _ -> error "something bad"

pMorphismTermSimple :: Parser MorphismTerm
pMorphismTermSimple =
    do
        m_type <- pMorphismTermType
        category <- pCategory
        return MorphismTerm{
            m_type=m_type,
            m_category=category
        }

pIntermediateMorphism :: Parser Category
pIntermediateMorphism = do
    let expr_parser = makeExprParser pMorphismTermSimple [[binaryR (spaceConsumer <* symbol (pack "->")) MorphismTermChain]]
    result <- expr_parser
    return IntermediateMorphism{
        chain = uncurryMorphismTermChain result
    }

pMorphism :: Parser Category
pMorphism = pIntermediateMorphism
