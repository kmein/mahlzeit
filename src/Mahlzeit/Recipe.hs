{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Mahlzeit.Recipe where

import           Control.Applicative            ( empty )
import           Data.Text                      ( Text )
import           Data.Yaml                      ( FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                , Value(String)
                                                )
import           GHC.Generics                   ( Generic )
import           Numeric.Natural                ( Natural )

data Recipe
  = Recipe
  { title :: Text
  , source :: Maybe Text
  , scale :: Double
  , tags :: [Text]
  , ingredients :: [Ingredient]
  , method :: [Text]
  , nutrients :: Maybe Nutrients
  } deriving (Show, Generic, Eq)

instance FromJSON Recipe
instance ToJSON Recipe

data Nutrients
  = Nutrients
  { kcal :: Maybe Double
  , protein :: Maybe Double
  , fat :: Maybe Double
  , carbohydrates :: Maybe Double
  } deriving (Show, Generic, Eq)

instance FromJSON Nutrients
instance ToJSON Nutrients

data Ingredient
  = Ingredient
  { ingredient :: Text
  , amount :: Double
  , unit :: Maybe Unit
  , note :: Maybe Text
  } deriving (Show, Generic, Eq)

instance FromJSON Ingredient
instance ToJSON Ingredient

data Unit
  = Gram
  | Teaspoon
  | Tablespoon
  | Milliliter
  deriving (Show, Eq)

instance FromJSON Unit where
  parseJSON (String text) = case text of
    "g" -> pure Gram
    "ts" -> pure Teaspoon
    "tbs" -> pure Tablespoon
    "ml" -> pure Milliliter
    _ -> empty
  parseJSON _ = empty

instance ToJSON Unit where
  toJSON = \case
    Gram -> String "g"
    Teaspoon -> String "ts"
    Tablespoon -> String "tbs"
    Milliliter -> String "ml"

type RecipeID = String

rescale :: Double -> Recipe -> Recipe
rescale factor recipe = recipe
  { ingredients = map (\ingredient -> ingredient { amount = adjust (amount ingredient) })
                      (ingredients recipe)
  , scale       = factor
  , nutrients   = (\n -> n { kcal = (* factor) <$> kcal n
                           , fat = (* factor) <$> fat n
                           , carbohydrates = (* factor) <$> carbohydrates n
                           , protein = (* factor) <$> protein n
                           }) <$> nutrients recipe
  }
  where
    adjust = (* factor) . (/ scale recipe)

