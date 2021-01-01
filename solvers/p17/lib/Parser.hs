module Parser where

import           Data.Either.Extended
import           Data.List                     (isPrefixOf)
import           Data.Ix                       (range)
import           Data.Set                      (Set, fromList)
import           Data.Tuple                    (swap)
import           Text.ParserCombinators.Parsec

underground :: [String] -> Either ParseError (Set (Int,Int))
underground = fmap mapUnderground . foldAll . fmap (parse vein "vein") . stripComments

stripComments :: [String] -> [String]
stripComments (l:ls) | "#" `isPrefixOf` l = stripComments ls
stripComments (l:ls)                      = l : stripComments ls
stripComments []                          = []

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
number = read <$> many1 digit
