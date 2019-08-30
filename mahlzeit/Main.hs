{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import           Control.Monad                  ( guard
                                                , forM_
                                                )
import           Data.List                      ( isSubsequenceOf )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import           Data.Yaml                      ( decodeFileThrow )
import           Numeric.Natural                ( Natural )
import           Options.Applicative
import           System.Console.ANSI
import           System.Directory               ( listDirectory
                                                , doesDirectoryExist
                                                )
import           System.Environment             ( getArgs
                                                , getEnv
                                                )
import           System.FilePath.Posix          ( (</>)
                                                , takeExtension
                                                , takeBaseName
                                                , (<.>)
                                                )
import           Text.Printf                    ( printf )
import qualified Data.Text
import qualified Data.Text.IO

import           Mahlzeit.Find
import           Mahlzeit.PrettyPrinter
import           Mahlzeit.Recipe


data SearchOptions
  = SearchOptions
  { searchTitle :: Maybe Text
  , searchTags :: Maybe [Text]
  , searchIngredients :: Maybe [Text]
  }

data MahlzeitCommand
  = Display RecipeID (Maybe Double)
  | Search SearchOptions

mahlzeitCommand :: Parser MahlzeitCommand
mahlzeitCommand = subparser $ mconcat
  [ command "list" $ info (searchParser <**> helper) $ progDesc "List available recipes"
  , command "show" $ info (showParser <**> helper) $ progDesc "Show/rescale a recipe"
    -- TODO: create 
    -- TODO: import
    -- TODO: edit
  ]
 where
  searchParser = do
    searchTitle       <- optional $ strOption (long "title" <> help "Search for parts of the title")
    searchTags        <- optional $ some $ strOption (long "tag" <> help "Search for set tags")
    searchIngredients <- optional $ some $ strOption (long "ingredient" <> help "Search for used ingredients")
    pure $ Search $ SearchOptions {..}
  showParser = Display <$> strArgument (metavar "ID") <*> optional (argument auto (metavar "SERVINGS"))

searchPredicate :: SearchOptions -> Recipe -> Bool
searchPredicate SearchOptions {..} Recipe {..}
  = let
      titleOk = maybe True (title `contains`) searchTitle
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
    Display recipeId factor -> do
      recipe <- findById recipeId
      let recipe' = maybe recipe (`rescale` recipe) factor
      putDoc $ pretty recipe'
