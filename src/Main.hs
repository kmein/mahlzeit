{-# LANGUAGE FlexibleContexts #-}
module Main where

import Parser (parseMeal)
import Types
import PrettyPrinter (renderMeal)

import Text.Megaparsec
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = do
  input <- Text.getContents
  case parse parseMeal "(stdin)" input of
    Right meal -> Text.putStrLn $ renderMeal meal
    Left e -> print e
