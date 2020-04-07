module Mahlzeit.Find
  ( findAll
  , findById
  , find
  , recipePath
  )
where

import           Control.Monad                  ( when
                                                , unless
                                                , (<=<)
                                                , guard
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Yaml                      ( decodeFileThrow
                                                , encodeFile
                                                )
import           System.Directory               ( createDirectory
                                                , doesDirectoryExist
                                                , doesFileExist
                                                , listDirectory
                                                )
import           System.Environment             ( getEnv
                                                , lookupEnv
                                                )
import           System.FilePath.Posix

import           Mahlzeit.Recipe                ( RecipeID
                                                , Recipe
                                                )

defaultHome :: IO FilePath
defaultHome = (</> "mahlzeit") <$> getEnv "HOME"

confirmYesNo :: String -> IO Bool
confirmYesNo question = do
  putStr (question ++ " (Y/n) ")
  answer <- getLine
  return $ answer == "Y"

createIfNotExist :: FilePath -> IO ()
createIfNotExist path = do
  pathExists <- doesDirectoryExist path
  unless pathExists $ do
    doCreate <- confirmYesNo $ "Create directory " ++ path ++ "?"
    when doCreate $ createDirectory path

recipeHome :: IO FilePath
recipeHome = do
  home' <- defaultHome
  home  <- fromMaybe home' <$> lookupEnv "RECIPE_HOME"
  createIfNotExist home
  pure home

recipePath :: RecipeID -> IO FilePath
recipePath recipeId = (</> recipeId <.> "yml") <$> recipeHome

findAll :: IO [(RecipeID, Recipe)]
findAll = do
  home  <- recipeHome
  paths <-
    map (home </>)
    .   filter (\file -> takeExtension file == ".yml")
    <$> listDirectory home
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
