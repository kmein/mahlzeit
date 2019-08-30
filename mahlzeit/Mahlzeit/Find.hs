module Mahlzeit.Find
  ( findAll
  , findById
  , find
  )
where

import           Control.Monad                  ( guard
                                                , (<=<)
                                                )
import           Data.Yaml                      ( decodeFileThrow
                                                , encodeFile
                                                )
import           System.Directory               ( doesDirectoryExist
                                                , doesFileExist
                                                , listDirectory
                                                )
import           System.Environment             ( getEnv )
import           System.FilePath.Posix

import           Mahlzeit.Recipe                ( RecipeID
                                                , Recipe
                                                )

recipeHome :: IO FilePath
recipeHome = do
  home <- getEnv "RECIPE_HOME"
  guard =<< doesDirectoryExist home
  pure home

recipePath :: RecipeID -> IO FilePath
recipePath recipeId = (</> recipeId <.> "yml") <$> recipeHome

findAll :: IO [(RecipeID, Recipe)]
findAll = do
  home    <- recipeHome
  paths   <- map (home </>) . filter (\file -> takeExtension file == ".yml") <$> listDirectory home
  recipes <- traverse decodeFileThrow paths
  pure $ zip (map takeBaseName paths) recipes

create :: RecipeID -> Recipe -> IO ()
create recipeId recipe = do
  path <- recipePath recipeId
  guard =<< (not <$> doesFileExist path)
  encodeFile path recipe

findById :: RecipeID -> IO Recipe
findById = decodeFileThrow <=< recipePath

find :: (Recipe -> Bool) -> IO [(RecipeID, Recipe)]
find predicate = filter (predicate . snd) <$> findAll
