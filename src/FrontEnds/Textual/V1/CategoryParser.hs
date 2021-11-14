{-# LANGUAGE OverloadedStrings #-}
module FrontEnds.Textual.V1.CategoryParser (parseCategoryFile, parseCategoryString) where

import CategoryData

import Text.Megaparsec
import Control.Monad.Combinators.Expr
import Text.Megaparsec.Char
import Text.Megaparsec.Debug (dbg)
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Text (Text)
import Data.Void
import Data.Maybe (fromJust, isJust)

import Debug.Trace (trace) 

import System.IO
import Data.Text (pack, unpack)

parseCategoryFile :: FilePath -> Text -> Category 
parseCategoryFile file_path file_contents = do
    let result = runParser pCategory file_path file_contents
    case result of
        Left parse_error -> error (show parse_error)
        Right result -> result

parseCategoryString :: Text -> Category
parseCategoryString = parseCategoryFile "String"

type Parser = Parsec Void Text

{- megaparsec helpers -}
spaceConsumer :: Parser ()
spaceConsumer = L.space
                space1                         -- (2)
                (L.skipLineComment "//")       -- (3)
                (L.skipBlockComment "/*" "*/") -- (4)

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
    _ <- symbol ":"
    return name

pCategoryOptionalLabel :: Parser Id
pCategoryOptionalLabel = do
    result <- optional pCategoryLabel
    case result of
        Just inner_val -> return inner_val
        Nothing -> return Unnamed 

pCategoryInnerList :: Parser [Category]
pCategoryInnerList = sepBy pCategory (symbol ",")
{- end category helpers-}

pCategory = pCategoryExpr

pCategoryExpr :: Parser Category 
pCategoryExpr = 
    let
        expr_parser = makeExprParser (pCategoryTerm) [
                [
                    binary (symbol "::") (\x y -> Membership{big_category=x,small_category=y})
                ],
                [
                    binary (spaceConsumer <* symbol "->") (\x y -> Morphism{name=Unnamed,input=x,output=y})
                ]
            ]
    in
        do
            name <- pCategoryOptionalLabel
            expr <- expr_parser
            case name of
                Unnamed -> return expr
                _ -> return expr{name=name}
    
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
    -- case extension of
    --     _ -> error "unsupported"

pCategoryBase :: Parser Category
pCategoryBase = choice [
        pUniversal,
        pFlexible,
        pReference,
        pThing,
        pComposition,
        pSumposition,
        pTuple,
        pSumple,
        pHigher,
        pPlaceholderAndRefinement
    ]

pCategoryExtension :: Parser Category 
pCategoryExtension = choice [
        pMorphismCallExtension,
        pDereferenceExtension
    ]

pThing :: Parser Category
pThing = do
    _ <- "`"
    thing_name <- pCategoryName 
    return Thing{name=thing_name}

pTuple :: Parser Category
pTuple = do 
    name <- pCategoryOptionalLabel
    inner_categories <- between (symbol "(") (symbol ")") pCategoryInnerList
    case inner_categories of
        [something] -> return something
        _ -> return Composite{name=name, composition_type=Product, inner=inner_categories}

pSumple :: Parser Category
pSumple = do 
    name <- pCategoryOptionalLabel
    inner_categories <- between (symbol "|") (symbol "|") pCategoryInnerList
    case inner_categories of
        [something] -> return something
        _ -> return Composite{name=name, composition_type=Sum, inner=inner_categories}

pHigher :: Parser Category
pHigher = do 
    name <- pCategoryOptionalLabel
    inner_categories <- between (symbol "^{") (symbol "}") pCategoryInnerList
    return Composite{name=name, composition_type=Higher, inner=inner_categories}

pComposition :: Parser Category
pComposition = do
    name <- pCategoryOptionalLabel
    inner_categories <- between (symbol "*(") (symbol ")") pCategoryInnerList
    if all isMorphic inner_categories
        then case inner_categories of
            [something] -> return something
            _ -> return Composite{name=name, composition_type=Composition, inner=inner_categories}
        else fail "Not a composition of morphisms"

pSumposition :: Parser Category
pSumposition = do
    name <- pCategoryOptionalLabel
    inner_categories <- between (symbol "*|") (symbol "|") pCategoryInnerList
    if all isMorphic inner_categories
        then case inner_categories of
            [something] -> return something
            _ -> return Composite{name=name, composition_type=Sumposition, inner=inner_categories}
        else fail "Not a composition of morphisms"

pPlaceholderInner :: Parser Category 
pPlaceholderInner = do
    name <- pCategoryName
    level <- optional (between (symbol "<") (symbol ">") integer)
    let ph_level = if isJust level then Just (fromInteger (fromJust level)) else Nothing
    _ <- symbol "@"
    category <- between (symbol "(") (symbol ")") pCategory
    return Placeholder{name=name,ph_level=ph_level,ph_category=category}

pRefinementInner :: Parser Category
pRefinementInner = do
    ph <- pPlaceholderInner
    _ <- symbol "|"
    refinement <- pCategory
    return RefinedCategory{name=Unnamed,base_category=ph,predicate=refinement}

pPlaceholderAndRefinement :: Parser Category
pPlaceholderAndRefinement = do
    refinement_name <- pCategoryOptionalLabel
    result <- between (symbol "{") (symbol "}") (try pRefinementInner <|> pPlaceholderInner)
    case result of
        r@RefinedCategory{} -> return r{name=refinement_name}
        _ -> return result

pFlexible :: Parser Category
pFlexible = do
    name <- pCategoryOptionalLabel
    _ <- symbol "(%)"
    return (Special name Flexible)

pUniversal :: Parser Category
pUniversal = do
    name <- pCategoryOptionalLabel
    _ <- symbol "%Any"
    return (Special name Universal)

pReference :: Parser Category 
pReference = do
    _ <- symbol "$"
    name <- pCategoryName
    return Special{name=name,special_type=Reference}


pMorphismCallExtension :: Parser Category 
pMorphismCallExtension = do
    argument <- between (symbol "[") (symbol "]") pCategory
    return MorphismCall{base_morphism=Thing Unnamed,argument=argument}

pDereferenceExtension :: Parser Category
pDereferenceExtension = do
    _ <- symbol "."
    category_id <- pCategoryName
    return Dereference{base_category=Thing Unnamed, category_id=category_id}

-- TODO: let/where, slicing