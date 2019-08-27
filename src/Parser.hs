{-# LANGUAGE RecordWildCards #-}
module Parser where

import Text.Megaparsec

import Types (Meal(..))

parseMeal :: Parser Meal
parseMeal = do
  parseHeader >> crlf
  title <- parseTitle >> crlf
  pure Meal{..}
  where 
    parseHeader = do
      count 5 (char '-') 
      many anyChar
      text "Meal-Master"
      many anyChar
    parseTitle = do
      count' 0 5 anyChar
      text "Title: "
      count' 0 60 anyChar
    parseCategories = do
      many anyChar
      text "Categories: "
      parseCategory `sepBy` (char "," >> many space)
