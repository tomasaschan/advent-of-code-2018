module Solvers.Dec17 where

import Data.Ix (range)
import Data.Tuple (swap)
import Text.ParserCombinators.Parsec

type Vein = ([Int], [Int])

vein :: GenParser Char st ([Int],[Int])
vein = vertical <|> horizontal

vertical :: GenParser Char st ([Int],[Int])
vertical = try $ (,) <$> (xs <* string ", ") <*> ys
horizontal :: GenParser Char st ([Int],[Int])
horizontal = try $ fmap swap $ (,) <$> (ys <* string ", ") <*> xs

xs :: GenParser Char st [Int]
xs = string "x=" *> rng
ys :: GenParser Char st [Int]
ys = string "y=" *> rng

rng :: GenParser Char st [Int]
rng = coords <|> fmap pure number

coords :: GenParser Char st [Int]
coords = try $ fmap range $ (,) <$> number <* string ".." <*> number

number :: GenParser Char st Int
number = fmap read $ many1 digit
