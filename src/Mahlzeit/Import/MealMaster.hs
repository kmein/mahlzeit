{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Mahlzeit.Import.MealMaster
  ( parseMealMaster
  )
where

import           Control.Applicative            ( liftA2 )
import           Control.Monad                  ( guard )
import           Data.List                      ( isInfixOf )
import           Data.Maybe                     ( fromMaybe )
import           Data.Ratio                     ( (%) )
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import           Numeric.Natural                ( Natural )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer     ( float
                                                , decimal
                                                )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

import           Mahlzeit.Recipe

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

parseTags :: Parser [Text]
parseTags = do
  optional space
  string "Categories:"
  space
  sepBy1 parseTag (char ',' *> optional space)
  where parseTag = Text.pack <$> some (letterChar <|> char ' ')

parseScale :: Parser Double
parseScale = do
  optional space
  string "Yield:"
  space
  fromIntegral <$> decimal <* (anySingle `manyTill` newline)

parseQuantity :: Parser Double
parseQuantity = try parseFraction <|> try parseFloating <|> parseWhole
 where
  parseFloating = float
  parseWhole    = fromIntegral <$> decimal

parseFraction :: Parser Double
parseFraction = do
  firstNumber <- fromIntegral <$> decimal
  (do
      space
      numerator <- fromIntegral <$> decimal
      char '/'
      denominator <- fromIntegral <$> decimal
      pure (firstNumber + numerator / denominator)
    )
    <|> (do
          char '/'
          denominator <- fromIntegral <$> decimal
          pure (firstNumber / denominator)
        )

parseUnit :: Parser (Either Double Text, Maybe Unit)
parseUnit =
  ((Left 1, Just Tablespoon) <$ (string "tb" <|> string "T"))
    <|> ((Left 1, Just Teaspoon) <$ (string "ts" <|> string "t"))
    <|> ((Right "small", Nothing) <$ string "sm")
    <|> ((Right "medium", Nothing) <$ string "md")
    <|> ((Right "large", Nothing) <$ string "lg")
    <|> ((Right "pinch", Nothing) <$ string "pn")
    <|> ((Left 0.62, Just Milliliter) <$ string "ds")
    <|> ((Right "package", Nothing) <$ string "pk")
    <|> ((Left 473.18, Just Milliliter) <$ string "pt")
    <|> ((Left 1136.52, Just Milliliter) <$ string "qt")
    <|> ((Left 284.13, Just Milliliter) <$ string "c")
    <|> ((Left 28.34, Just Gram) <$ string "oz")
    <|> ((Left 453.59, Just Gram) <$ string "lb")
    <|> ((Left 1, Nothing) <$ (string "x" <|> string "ea"))

{-
 - x per serving
 - sm small
 - md medium
 - lg large
 - cn can
 - pk package
 - pn pinch
 - dr drop
 - ds dash
 - ct carton
 - bn bunch
 - sl slice
 - ea each
 - t/ts teaspoon
 - T/tb tablespoon
 - fl fluid ounce
 - c cup
 - pt pint
 - qt quart
 - ga gallon
 - oz ounce
 - lb pound
 - ml milliliter
 - cc cubic cm
 - cl centiliter
 - dl deciliter
 - l liter
 - mg milligram
 - cg centigram
 - dg decigram
 - g gram
 - kg kilogram
 -}

parseName :: Parser Text
parseName = Text.pack <$> anySingle `someTill` newline

parseIngredient :: Parser Ingredient
parseIngredient = do
  optional space
  q <- parseQuantity
  space
  (factorOrNote, unit) <- parseUnit
  let (factor, note) = case factorOrNote of
        Left  f -> (f, Nothing)
        Right n -> (1, Just n)
      amount = q * factor
  space
  n <- parseName
  c <- optional $ try $ do
    optional space
    char '-'
    Text.pack <$> anySingle `someTill` newline
  let ingredient = n <> maybe "" (" " <>) c
  pure Ingredient {..}

parseRecipe :: Parser Recipe
parseRecipe = do
  ()          <- parseHeader
  title       <- parseTitle
  tags        <- parseTags
  scale       <- parseScale
  ingredients <- some (try parseIngredient)
  method      <-
    map (Text.replace "\n" " ")
    .          Text.splitOn "\n\n"
    .          Text.strip
    .          Text.pack
    <$>        anySingle
    `someTill` try parseFooter
  let source = Nothing
  pure Recipe {..}

parseFromFile :: Parser a -> FilePath -> IO (Either (ParseErrorBundle Text Void) a)
parseFromFile p file = runParser p file <$> Text.readFile file

parseMealMaster :: FilePath -> IO (Either (ParseErrorBundle Text Void) Recipe)
parseMealMaster = parseFromFile parseRecipe
