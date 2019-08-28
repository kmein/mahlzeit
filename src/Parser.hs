{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Parser where

import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Void (Void)
import Numeric.Natural (Natural)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (float, decimal)
import qualified Data.Text as Text

import Types (Meal(..), Ingredient(..))

type Parser = Parsec Void Text

parseHeader :: Parser ()
parseHeader = do
  count 5 (char 'M')
  count 5 (char '-') 
  rest <- anySingle `manyTill` newline
  guard $ "Meal-Master" `isInfixOf` rest

parseFooter :: Parser ()
parseFooter = do
  count 5 (char 'M')
  optional newline
  () <$ eof

parseTitle :: Parser Text
parseTitle = do
  optional space
  string "Title:"
  space
  Text.pack <$> anySingle `someTill` newline

parseCategories :: Parser [Text]
parseCategories = do
  optional space
  string "Categories:"
  space
  sepBy1 parseCategory (char ',' *> optional space) 
  where 
    parseCategory = Text.pack <$> some letterChar

parseYield :: Parser Natural
parseYield = do
  optional space
  string "Yield:"
  space
  fromIntegral <$> decimal <* (anySingle `manyTill` newline)

-- works
parseQuantity :: Parser Rational
parseQuantity = try parseFraction <|> try parseFloating <|> parseWhole
  where 
    parseFloating :: Parser Rational
    parseFloating = toRational <$> float
    parseWhole :: Parser Rational
    parseWhole = fromIntegral <$> decimal

parseFraction :: Parser Rational
parseFraction = do 
  firstNumber <- decimal
  (do space
      numerator <- decimal
      char '/'
      denominator <- decimal
      pure (fromIntegral firstNumber + numerator % denominator)) 
    <|> (do char '/'
            denominator <- decimal
            pure (firstNumber % denominator))

parseUnit :: Parser Text
parseUnit = Text.pack <$> some letterChar

parseName :: Parser Text
parseName = Text.pack <$> anySingle `someTill` newline

parseIngredient :: Parser Ingredient
parseIngredient = do
  optional space
  q <- optional parseQuantity
  space
  u <- parseUnit
  space
  n <- parseName
  c <- optional $ try $ do
    optional space
    char '-'
    Text.pack <$> anySingle `someTill` newline
  pure $ Ingredient q u (n <> maybe "" (" " <>) c)


parseHeaderBlock :: Parser (Text, [Text], Natural)
parseHeaderBlock = (,,) <$> parseTitle <*> parseCategories <*> parseYield

parseDirections :: Parser [String]
parseDirections = paragraph `sepBy1` newline
  where 
    paragraph :: Parser String
    paragraph = concat <$> some (anySingle `someTill` newline)

parseMeal :: Parser Meal
parseMeal = do
  () <- parseHeader 
  title <- parseTitle 
  categories <- parseCategories
  yield <- parseYield
  ingredients <- some parseIngredient
  directionsStr <- parseDirections
  parseFooter
  pure Meal{..}
