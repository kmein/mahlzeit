module Mahlzeit.Find (findAll, findById, find) where 

import Control.Monad (guard)
import Data.Yaml (decodeFileThrow)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getEnv)
import System.FilePath.Posix

import Mahlzeit.Recipe (RecipeID, Recipe)

recipeHome :: IO FilePath
recipeHome = do
  home <- getEnv "RECIPE_HOME"
  guard =<< doesDirectoryExist home
  pure home

findAll :: IO [(RecipeID, Recipe)]
findAll = do 
  home <- recipeHome
  paths <- map (home </>) . filter (\file -> takeExtension file == ".yml") <$> listDirectory home
  recipes <- traverse decodeFileThrow paths
  pure $ zip (map takeBaseName paths) recipes

findById :: RecipeID -> IO Recipe
findById recipeId = do
  home <- recipeHome
  let path = home </> recipeId <.> "yml"
  decodeFileThrow path

find :: (Recipe -> Bool) -> IO [(RecipeID, Recipe)]
find predicate = filter (predicate . snd) <$> findAll
