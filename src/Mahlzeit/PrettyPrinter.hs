{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Mahlzeit.PrettyPrinter
  ( putDoc
  , pretty
  , recipeList
  )
where

import           Data.Ratio                     ( numerator
                                                , denominator
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Numeric.Natural                ( Natural )
import           System.Console.ANSI
import           Text.Printf
import           Data.Maybe                     ( catMaybes )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Util ( reflow )
import           Data.Text.Prettyprint.Doc.Render.Text
                                                ( putDoc )
import qualified Data.Text

import           Mahlzeit.Recipe

recipeList :: [(RecipeID, Recipe)] -> Doc ann
recipeList = vcat . map (uncurry prettyEntry)

prettyEntry :: RecipeID -> Recipe -> Doc ann
prettyEntry recipeId Recipe {..} =
  brackets (sgr [SetColor Foreground Dull Cyan] $ pretty recipeId) <+> pretty title <+> sgr
    [SetColor Foreground Vivid Black]
    (commaSep (map pretty tags))

prettyDouble :: Double -> Doc ann
prettyDouble x =
  let f      = toRational x
      (n, d) = (numerator f, denominator f)
      (q, r) = n `quotRem` d
  in  (if q /= 0 then pretty q else emptyDoc) <+> (if r /= 0 then pretty r <> "/" <> pretty d else emptyDoc)

commaSep :: [Doc ann] -> Doc ann
commaSep = encloseSep emptyDoc emptyDoc (comma <> space)

pipeSep :: [Doc ann] -> Doc ann
pipeSep = encloseSep emptyDoc emptyDoc (space <> "|" <> space)

markdownDescription :: Doc ann -> Doc ann -> Doc ann
markdownDescription key value = key <> hardline <> colon <+> value <> hardline

instance Pretty Nutrients where
  pretty Nutrients{..} = 
    markdownDescription "Nutrients" $ pipeSep $ catMaybes 
      [ fmap (\n -> round' n <+> "kcal") kcal
      , fmap (\n -> "carbs" <+> round' n <+> "g") carbohydrates
      , fmap (\n -> "protein" <+> round' n <+> "g") protein
      , fmap (\n -> "fat" <+> round' n <+> "g") fat
      ]
    where round' = pretty . round @Double @Int

instance Pretty Recipe where
  pretty Recipe{..} = vsep
    [ "#" <+> pretty title <> maybe emptyDoc (\s -> "^" <> brackets (pretty s)) source
    , markdownDescription "Tags" $ commaSep (map pretty tags) 
    , markdownDescription "Yield" $ prettyDouble scale
    , maybe emptyDoc pretty nutrients
    , softline
    , "##" <+> "Ingredients"
    , vcat (map pretty ingredients)
    , softline
    , "##" <+> "Method"
    , vsep $ zipWith (\i step -> pretty @Natural i <> dot <+> align (reflow step)) [1..] method
    ]

instance Pretty Unit where
  pretty = \case
    Gram       -> "g"
    Milliliter -> "ml"
    Tablespoon -> "tbsp"
    Teaspoon   -> "tsp"

instance Pretty Ingredient where
  pretty Ingredient{..} =
    "-"
    <+> fill 4 (prettyDouble amount)
    <+> fill 4 (maybe emptyDoc pretty unit)
    <+> reflow ingredient
    <+> maybe emptyDoc (parens . pretty) note

sgr :: [SGR] -> Doc ann -> Doc ann
sgr codes t = pretty (setSGRCode codes) <> t <> pretty (setSGRCode [])
