module Mahlzeit.Recipe where

import Data.Text (Text)
import Numeric.Natural (Natural)

data Recipe
  = Recipe
  { title :: Text
  , source :: Maybe Text
  , author :: Maybe Text
  , scale :: Natural
  , tags :: [Text]
  , ingredients :: [Ingredient]
  , method :: [Text]
  } deriving (Show)

data Ingredient
  = Ingredient
  { ingredient :: Text
  , amount :: Rational
  , unit :: Maybe Unit
  , note :: Maybe Text
  } deriving (Show)

data Unit
  = Gram
  | Teaspoon
  | Tablespoon
  | Milliliter
  deriving (Show)
