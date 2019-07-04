module Solver where

import           Data.Either.Extended
import           Data.Foldable.Extended
import           Data.Ix                       (range)
import           Data.Set                      (Set, fromList)
import           Data.Tuple                    (swap)
import           Text.ParserCombinators.Parsec
import           Text.Printf

solveA :: [String] -> String
solveA =
  either show draw . fmap mapUnderground . foldAll . fmap (parse vein "vein")

draw :: Set (Int, Int) -> String
draw underground =
  let ((minx, miny), (maxx, maxy)) = mapSize underground
      xs = [minx - 1 .. maxx + 1]
      ys = [(min 0 miny) .. maxy]
      square 500 0 = '+'
      square x y =
        if (x, y) `elem` underground
          then '#'
          else '.'
      line y = printf "%3d " y ++ [square x y | x <- xs]
      hundreds x = x `div` 100
      tens x = (x `mod` 100) `div` 10
      ones x = (x `mod` 10)
      row100 = "    " ++ foldMap (show . hundreds) xs
      row10 = "    " ++ foldMap (show . tens) xs
      row1 = "    " ++ foldMap (show . ones) xs
   in unlines $ [row100, row10, row1] ++ fmap line ys

mapUnderground :: [([Int], [Int])] -> Set (Int, Int)
mapUnderground = foldMap mapVein

mapVein :: ([Int], [Int]) -> Set (Int, Int)
mapVein (x, y) = fromList [(x', y') | x' <- x, y' <- y]

vein :: GenParser Char st ([Int], [Int])
vein = vertical <|> horizontal

vertical :: GenParser Char st ([Int], [Int])
vertical = try $ (,) <$> (pXs <* string ", ") <*> pYs

horizontal :: GenParser Char st ([Int], [Int])
horizontal = try $ fmap swap $ (,) <$> (pYs <* string ", ") <*> pXs

pXs :: GenParser Char st [Int]
pXs = string "x=" *> rng

pYs :: GenParser Char st [Int]
pYs = string "y=" *> rng

rng :: GenParser Char st [Int]
rng = coords <|> fmap pure number

coords :: GenParser Char st [Int]
coords = try $ fmap range $ (,) <$> number <* string ".." <*> number

number :: GenParser Char st Int
number = fmap read $ many1 digit
