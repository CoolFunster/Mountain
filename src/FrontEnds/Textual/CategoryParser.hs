{-# LANGUAGE OverloadedStrings #-}
module FrontEnds.Textual.CategoryParser where

import CategoryData

import Text.Megaparsec
import Control.Monad.Combinators.Expr
import Text.Megaparsec.Char
import Text.Megaparsec.Debug (dbg)
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Text (Text)
import Data.Void
import Data.Maybe (fromJust)

import Debug.Trace (trace)

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

binary  inner_parser f = InfixL  (f <$ inner_parser)
prefix  inner_parser f = Prefix  (f <$ inner_parser)
postfix inner_parser f = Postfix (f <$ inner_parser)

integer :: Parser Integer
integer = lexeme L.decimal 
{- megaparsec helpers end -}

{- category helpers-}
pCategoryName :: Parser Id
pCategoryName = do
    name <- some letterChar 
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
        expr_parser = makeExprParser pCategoryTerm [
                [binary (spaceConsumer <* symbol "->") (\x y -> Morphism{name=Unnamed,input=x,output=y})]
            ]
    in
        do
            name <- pCategoryOptionalLabel
            expr <- expr_parser
            case name of
                Unnamed -> return expr
                _ -> return expr{name=name}
    
pCategoryTerm :: Parser Category
pCategoryTerm = choice [
        pThing,
        -- pPlaceholder
        pComposition,
        pSumposition,
        pTuple,
        pSumple,
        pHigher
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
    return Composite{name=name, composition_type=Product, inner=inner_categories}

pSumple :: Parser Category
pSumple = do 
    name <- pCategoryOptionalLabel
    inner_categories <- between (symbol "|") (symbol "|") pCategoryInnerList
    return Composite{name=name, composition_type=Sum, inner=inner_categories}

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
        then return Composite{name=name, composition_type=Composition, inner=inner_categories}
        else fail "Not a composition of morphisms"

pSumposition :: Parser Category
pSumposition = do
    name <- pCategoryOptionalLabel
    inner_categories <- between (symbol "*|") (symbol "|") pCategoryInnerList
    if all isMorphic inner_categories
        then return Composite{name=name, composition_type=Sumposition, inner=inner_categories}
        else fail "Not a composition of morphisms"

pPlaceholder :: Parser Category
pPlaceholder = do
    name <- pCategoryName
    level <- optional (between (symbol "<") (symbol ">") integer)
    _ <- symbol "@"
    category <- pCategory
    case level of
        Nothing -> return Placeholder{name=name,ph_level=Nothing,ph_category=category}
        Just some_integer -> return Placeholder{name=name,ph_level=Just (fromInteger some_integer),ph_category=category}