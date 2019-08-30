{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Mahlzeit.PrettyPrinter where

import Data.Ratio (numerator, denominator)
import Data.Text (Text, pack, unpack)
import Numeric.Natural (Natural)
import System.Console.ANSI
import Text.Printf
import qualified Data.Text

import Mahlzeit.Recipe

shortInfo :: RecipeID -> Recipe -> Text
shortInfo recipeId Recipe{..} = 
  Data.Text.unwords 
  [ "[" <> withSGR [SetColor Foreground Dull Cyan] (pack recipeId) <> "]"
  , title 
  , withSGR [SetColor Foreground Vivid Black] $ Data.Text.intercalate ", " tags
  ]

{-
  display tags
  find out how to display author
-}
recipeInfo :: Recipe -> Text
recipeInfo Recipe {..} =
  let header =
        Data.Text.unlines
          [ "# " <> maybe title (\s -> "[" <> title <> "](" <> s <> ")") source
          , "Tags: " <> Data.Text.intercalate ", " tags 
          , ""
          , "Yield: " <> prettyDouble scale
          ]
      ingredientsSection =
        Data.Text.unlines $
          "## Ingredients" :
          map (\Ingredient{..} -> 
            let unitString = case unit of
                               Just Gram -> "g"
                               Just Milliliter -> "ml"
                               Just Tablespoon -> "tbs"
                               Just Teaspoon -> "ts"
                               Nothing -> ""
             in Data.Text.unwords ["*", prettyDouble amount, unitString, ingredient]) ingredients
      methodSection =
        Data.Text.unlines $
          "## Method" :
          zipWith (\(i :: Natural) step -> pack (show i) <> ". " <> step) [1..] method
  in Data.Text.unlines 
    [ header
    , ingredientsSection
    , methodSection
    ] 

withSGR :: [SGR] -> Text -> Text
withSGR sgr t = pack (setSGRCode sgr) <> t <> pack (setSGRCode [])

prettyDouble :: Double -> Text
prettyDouble x = 
  let f = toRational x
      (n, d) = (numerator f, denominator f)
      (q, r) = n `quotRem` d
   in 
     Data.Text.unwords $
       [pack (show q) | q /= 0] 
       <> [pack (show r) <> "/" <> pack (show d) | r /= 0] 
