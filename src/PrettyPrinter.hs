{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module PrettyPrinter where

import Data.Ratio (numerator, denominator)
import Data.List (intersperse)
import Data.Text (Text)
import Numeric.Natural (Natural)
import Prelude hiding ((<>))
import Text.PrettyPrint
import qualified Data.Text as Text (toUpper, pack, unpack)

import Types

prettyUnit :: Unit -> Doc
prettyUnit = text . \case
  Teaspoon -> "tsp"
  Tablespoon -> "tbsp"
  Small -> "sm"
  Medium -> "md"
  Large -> "lg"
  Cup -> "cup"
  Pinch -> "pinch"
  Dash -> "dash"
  Package -> "package"
  Pint -> "pt"
  Quart -> "qt"
  Pound -> "lb"
  Ounce -> "oz"

prettyIngredient :: Ingredient -> Doc
prettyIngredient Ingredient{..} = 
  maybe empty rational' quantity <+> maybe empty prettyUnit unit <+> text' name

prettyMeal :: Meal -> Doc
prettyMeal Meal{..} = 
  vcat 
  [ text' (Text.toUpper title) <+> parens (hcat $ intersperse (comma <> space) $ map text' categories)
  , text "Yield: " <> natural yield
  , empty
  , nest 2 $ vcat $ map prettyIngredient ingredients
  , empty
  , vcat $ map ((char '*' <+>) . nest 2 . text') directions
  ]

renderMeal :: Meal -> Text 
renderMeal = Text.pack . render . prettyMeal

text' :: Text -> Doc
text' = text . Text.unpack

natural :: Natural -> Doc
natural = int . fromIntegral

int' :: Integral a => a -> Doc
int' = int . fromIntegral

rational' :: Rational -> Doc
rational' x = (if q == 0 then empty else int' q) <+> (if r == 0 then empty else int' r <> char '/' <> int' d)
  where (n, d) = (numerator x, denominator x)
        (q, r) = n `quotRem` d
