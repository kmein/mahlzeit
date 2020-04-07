{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Mahlzeit.Export.Json where

import           Mahlzeit.Recipe
import           Data.Aeson
import           Mahlzeit.PrettyPrinter         ( )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.String

toJsonRecipe :: Recipe -> Value
toJsonRecipe Recipe {..} = object
  (  [ "@context" .= ("http://schema.org" :: String)
     , "@type" .= ("Recipe" :: String)
     , "recipeIngredient" .= map (show . pretty) ingredients
     , "name" .= title
     , "recipeYield" .= scale
     , "recipeInstructions" .= method
     ]
  ++ maybe
       []
       (\n ->
         [ "nutrition" .= object
             [ "@type" .= ("NutritionInformation" :: String)
             , "calories" .= kcal n
             , "fatContent" .= fat n
             , "carbohydrateContent" .= carbohydrates n
             , "proteinContent" .= protein n
             ]
         ]
       )
       nutrients
  )
