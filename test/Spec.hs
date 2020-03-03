{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit
import Test.DocTest
import Text.RawString.QQ
import Text.Megaparsec

import Mahlzeit.Recipe
import Mahlzeit.Import.MealMaster (parseRecipe)

main :: IO ()
main = do
  defaultMain $ testGroup "Tests" tests
  doctest ["src"]

exampleRecipe :: Recipe
exampleRecipe = Recipe
  { title = "Chili Con Carne"
  , source = Nothing
  , scale = 1
  , tags = ["Main dish", "Meats"]
  , ingredients = 
    [ Ingredient { ingredient = undefined
                 , amount = undefined
                 , unit = undefined
                 , note = Nothing
                 }
    ]
  , method = 
    [
    ]
  , nutrients = Nothing
  }

mealMasterExampleRecipe = [r|
MMMMM----- Recipe via Meal-Master (tm) v8.02

      Title: Chili Con Carne
 Categories: Main dish, Meats
      Yield: 1 servings

      1 lb ground beef
  1 1/4 c  minced onion
  2 1/2 c  cooked kidney beans
  1 1/3 c  condensed tomato soup
      3 T  chili powder
      1 T  flour
      3 T  water
      1 t  salt

Cook beef and onions until browned. Add the beans and soup and
cook for 10 or 15 minutes. Blend the chili powder, flour, water
and saltinto a paste and add in. Cook over low heat, stirring
frequently, 45 min. Serve hot.

MMMMM
|]



tests :: [TestTree]
tests = 
  [ testGroup "Mahlzeit.Import.MealMaster" 
    [ testCase "parsing" $ Just exampleRecipe @=? parseMaybe parseRecipe mealMasterExampleRecipe
    ]
  ]
