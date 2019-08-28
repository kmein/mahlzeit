module Types where

import Numeric.Natural (Natural)
import Data.Text (Text)

data Meal = Meal
  { title :: Text  -- maximum length: 60 characters
  , categories :: [Text]  -- maximum count: 5
  , yield :: Natural  -- from 1 to 9999
  , ingredients :: [Ingredient]
  , directions :: [Text]
  } deriving Show

data Ingredient = Ingredient
  { quantity :: Maybe Rational
  , unit :: Text
  , name :: Text
  } deriving Show
