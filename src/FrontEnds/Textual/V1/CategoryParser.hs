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

import System.IO


parseCategoryFileWith :: Parser a -> FilePath -> Text -> a
parseCategoryFileWith cat_parser file_path file_contents = do
    let result = runParser cat_parser file_path file_contents
    case result of
        Left parse_error -> error (errorBundlePretty parse_error)
        Right result -> result

parseCategoryFile :: FilePath -> Text -> Category
parseCategoryFile = parseCategoryFileWith pCategory

parseCategoryStringWith :: Parser a -> Text -> a
parseCategoryStringWith cat_parser = parseCategoryFileWith cat_parser "String"

parseCategoryString :: Text -> Category
parseCategoryString = parseCategoryFile "String"

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

binary  inner_parser f = InfixR  (f <$ inner_parser)
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

pCategoryLabel :: Parser Id
pCategoryLabel = do
    name <- pCategoryName
    _ <- symbol (pack ":")
    if name `elem` [Name "given", Name "import", Name "define", Name "return"]
        then fail "cannot name category keyword"
        else return name

pCategoryOptionalLabel :: Parser Id
pCategoryOptionalLabel = do
    result <- optional (try pCategoryLabel)
    case result of
        Just inner_val -> return inner_val
        Nothing -> return Unnamed

pCategoryInnerList :: Parser [Category]
pCategoryInnerList = sepBy pCategory (symbol (pack ","))
{- end category helpers-}

pCategory = pCategoryExpr

pCategoryExpr :: Parser Category
pCategoryExpr =
    let
        expr_parser = makeExprParser pCategoryTerm [
                [
                    binary (symbol (pack "::")) (\x y -> Membership{big_category=x,small_category=y})
                ]
            ]
    in
        do
            expr_parser

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

pCategoryBase :: Parser Category
pCategoryBase = try (pStandardLabeledCategory <|> pThing <|> pReference <|> pPlaceholder)

pStandardLabeledCategory :: Parser Category
pStandardLabeledCategory = do
    name <- pCategoryOptionalLabel
    category <- choice [
        pMorphism,
        pUniversal,
        pFlexible,
        pComposition,
        pSumposition,
        pTuple,
        pSumple,
        pHigher,
        pRefinement]
    case name of
        Unnamed -> return category
        _ -> return CategoryData.Label{name=name, target=category}


pCategoryExtension :: Parser Category
pCategoryExtension = choice [
        pMorphismCallExtension,
        pDereferenceExtension
    ]

pThing :: Parser Category
pThing = do
    _ <- symbol (pack "`")
    thing_name <- pCategoryName
    return Thing{name=thing_name}

pTuple :: Parser Category
pTuple = do
    inner_categories <- between (symbol (pack "(")) (symbol (pack ")")) pCategoryInnerList
    if length inner_categories == 1
        then return $ head inner_categories
        else return Composite{composition_type=Product, inner=inner_categories}

pSumple :: Parser Category
pSumple = do
    inner_categories <- between (symbol (pack "|")) (symbol (pack "|")) pCategoryInnerList
    return Composite{composition_type=Sum, inner=inner_categories}

pHigher :: Parser Category
pHigher = do
    inner_categories <- between (symbol (pack "^{")) (symbol (pack "}")) pCategoryInnerList
    return Composite{composition_type=Higher, inner=inner_categories}

pComposition :: Parser Category
pComposition = do
    inner_categories <- between (symbol (pack "*(")) (symbol (pack ")")) pCategoryInnerList
    if all isMorphic inner_categories
        then return Composite{composition_type=Composition, inner=inner_categories}
        else fail "Not a composition of morphisms"

pSumposition :: Parser Category
pSumposition = do
    inner_categories <- between (symbol (pack "*|")) (symbol (pack "|")) pCategoryInnerList
    if all isMorphic inner_categories
        then return Composite{composition_type=Sumposition, inner=inner_categories}
        else fail "Not a composition of morphisms"

pPlaceholder :: Parser Category
pPlaceholder = do
    name <- optional pCategoryName
    level <- optional (between (symbol (pack "<")) (symbol (pack ">")) integer)
    let ph_level = if isJust level then Just (fromInteger (fromJust level)) else Nothing
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
    category_id <- pCategoryName
    return Dereference{base_category=Thing Unnamed, category_id=category_id}

pMorphismTermType :: Parser MorphismTermType
pMorphismTermType =
    do
        pString <- choice [
            symbol $ pack "import",
            symbol $ pack "given",
            symbol $ pack "definition",
            symbol $ pack "return"]
        _ <- spaceConsumer
        case unpack pString of
            "import" -> return Import
            "given" -> return Given
            "definition" -> return Definition
            "return" -> return Return
            _ -> error "something bad"

pMorphismTermSimple :: Parser IntermediateMorphism
pMorphismTermSimple =
    do
        m_type <- pMorphismTermType
        category <- pCategory
        return MorphismTerm{
            m_type=m_type,
            m_category=category
        }


pIntermediateMorphism :: Parser IntermediateMorphism
pIntermediateMorphism =
    let
        expr_parser = makeExprParser pMorphismTermSimple [[binary (spaceConsumer <* symbol (pack "->")) MorphismChain]]
    in
        expr_parser

pMorphism :: Parser Category
pMorphism = do
    im <- pIntermediateMorphism
    let is_head_of_chain = True
    if validateIM is_head_of_chain im
        then return $ imToMorphism im
        else fail "invalid Morphism specification"

-- TODO: let/where, slicing