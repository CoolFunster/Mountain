{-# LANGUAGE OverloadedStrings #-}
module FrontEnds.Textual.CategoryParser where

import CategoryData

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Data.Void

type Parser = Parsec Void Text

