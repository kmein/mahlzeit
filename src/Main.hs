{-# LANGUAGE FlexibleContexts #-}
module Main where

import Parser 
import Types

import Text.Megaparsec
import Data.Text (Text)
import qualified Data.Text as Text

main :: IO ()
main = do
  input <- Text.pack <$> getContents
  parseTest parseMeal input
  -- case parse parseMeal "(stdin)" input of
  --   Right result -> print result
  --   Left e -> print e
