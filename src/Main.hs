{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import           Control.Monad                  ( guard
                                                , forM_
                                                )
import           Data.List                      ( isSubsequenceOf )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import           Data.Yaml                      ( decodeFileThrow
                                                , encodeFile
                                                )
import           Numeric.Natural                ( Natural )
import           Options.Applicative
import           System.Console.ANSI
import           System.Directory               ( listDirectory
                                                , doesDirectoryExist
                                                )
import           System.Environment             ( getArgs
                                                , getEnv
                                                , lookupEnv
                                                )
import           System.FilePath.Posix          ( (</>)
                                                , takeExtension
                                                , takeBaseName
                                                , (<.>)
                                                )
import           System.Process                 ( callCommand )
import           Text.Printf                    ( printf )
import qualified Data.Text
import qualified Data.Text.IO

import           Mahlzeit.Find
import           Mahlzeit.PrettyPrinter
import           Mahlzeit.Recipe
import           Mahlzeit.Import.MealMaster


data SearchOptions
  = SearchOptions
  { searchTitle :: Maybe Text
  , searchTags :: Maybe [Text]
  , searchIngredients :: Maybe [Text]
  }

data MahlzeitCommand
  = Display RecipeID (Maybe Double)
  | Search SearchOptions
  | Import FilePath
  | Edit RecipeID

mahlzeitCommand :: Parser MahlzeitCommand
mahlzeitCommand = subparser $ mconcat
  [ command "list" $ info (searchParser <**> helper) $ progDesc "List available recipes"
  , command "show" $ info (showParser <**> helper) $ progDesc "Show/rescale a recipe"
  , command "import" $ info (importParser <**> helper) $ progDesc "Import a Meal-Master file"
  , command "edit" $ info (editParser <**> helper) $ progDesc "Edit a recipe file"
    -- TODO: create 
  ]
 where
  searchParser = do
    searchTitle       <- optional $ strOption (long "title" <> help "Search for parts of the title")
    searchTags        <- optional $ some $ strOption (long "tag" <> help "Search for set tags")
    searchIngredients <- optional $ some $ strOption (long "ingredient" <> help "Search for used ingredients")
    pure $ Search $ SearchOptions {..}
  showParser   = Display <$> strArgument (metavar "ID") <*> optional (argument auto (metavar "SERVINGS"))
  importParser = Import <$> strArgument (metavar "PATH")
  editParser   = Edit <$> strArgument (metavar "ID")

searchPredicate :: SearchOptions -> Recipe -> Bool
searchPredicate SearchOptions {..} Recipe {..}
  = let
      titleOk = maybe True (title `contains`) searchTitle
      -- all ingredient searches occur in the recipe's ingredients
      ingredientsOk =
        maybe True (all (\search -> any ((`contains` search) . ingredient) ingredients)) searchIngredients
      tagsOk =
        maybe True (\ts -> map Data.Text.toLower ts `isSubsequenceOf` map Data.Text.toLower tags) searchTags
    in
      titleOk && ingredientsOk && tagsOk
  where x `contains` y = Data.Text.isInfixOf (Data.Text.toLower y) (Data.Text.toLower x)

main :: IO ()
main = do
  command <- execParser $ info (mahlzeitCommand <**> helper) fullDesc
  case command of
    Search options -> do
      recipes <- find $ searchPredicate options
      putDoc $ recipeList recipes
      putChar '\n'
    Display recipeId factor -> do
      recipe <- findById recipeId
      let recipe' = maybe recipe (`rescale` recipe) factor
      putDoc $ pretty recipe'
      putChar '\n'
    Edit recipeId -> do
      editor <- fromMaybe "vim" <$> lookupEnv "EDITOR"
      path   <- recipePath recipeId
      callCommand (editor <> " " <> path)
    Import path -> do
      parseResult <- parseMealMaster path
      case parseResult of
        Left  e      -> print e
        Right recipe -> do
          ymlPath <- recipePath $ takeBaseName path
          encodeFile ymlPath recipe
