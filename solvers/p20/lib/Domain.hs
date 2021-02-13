{-# LANGUAGE TupleSections #-}

module Domain where

import           Data.Function
import           Data.List -- (intercalate, intersperse)
import           Data.Foldable.Extended

import           Data.Map                       ( Map
                                                , (!?)
                                                )
import qualified Data.Map                      as Map
import qualified Data.Map.Merge.Lazy           as M

import           Text.Printf

data Direction = N | E | S | W
        deriving (Show, Eq, Ord)

one80 :: Direction -> Direction
one80 N = S
one80 S = N
one80 E = W
one80 W = E


data Step = Direction Direction
          | Fork [[Step]]
        deriving (Eq)
instance Show Step where
  show (Direction d) = show d
  show (Fork      f) = "Fork " ++ show f

type Coord = (Int, Int)
newtype Base = Base (Map Coord [Direction])

connect :: Direction -> Int -> Int -> Base -> Base
connect d x y (Base m) =
  Base $ Map.insertWith union (x, y) [d] $ Map.insertWith union
                                                          (walk (x, y) d)
                                                          [one80 d]
                                                          m

connectAll :: [Coord] -> Direction -> Base -> Base
connectAll pos d b = foldr (uncurry (connect d)) b pos

merge :: Base -> Base -> Base
merge (Base a) (Base b) = Base $ M.merge M.preserveMissing
                                         M.preserveMissing
                                         (M.zipWithMatched (const union))
                                         a
                                         b

door :: Int -> Int -> Direction -> Base -> Bool
door x y d (Base m) = d `elem` Map.findWithDefault mempty (x, y) m

neighbors :: Int -> Int -> Base -> [Coord]
neighbors x y (Base m) = walk (x, y) <$> Map.findWithDefault mempty (x, y) m

instance Show Base where
  show b =
    let Base m                       = b
        ((minx, miny), (maxx, maxy)) = mapSize $ Map.keys m

        above :: Int -> String
        above y =
            "#"
              ++ intersperse
                   '#'
                   [ if door x y N b then '-' else '#' | x <- [minx .. maxx] ]
              ++ "#"
        at :: Int -> String
        at y = intersperse
          '.'
          [ if door x y E b then '|' else '#' | x <- [minx - 1 .. maxx] ]
    in  intercalate
            "\n"
            [ printf "%s\n%s" (above y) (at y) | y <- [maxy, maxy - 1 .. miny] ]
          ++ "\n"
          ++ replicate (2 * (maxx - minx) + 3) '#'

createMap :: [Step] -> Base
createMap = snd . follow [(0, 0)] (Base Map.empty)
 where
  follow :: [Coord] -> Base -> [Step] -> ([Coord], Base)
  follow pos m [] = (pos, m)
  follow pos m (Direction d : steps) =
    let pos' = fmap (`walk` d) pos
        m'   = connectAll pos d m
    in  follow pos' m' steps
  follow pos m (Fork ps : steps) =
    let f'   = [ follow pos m f | f <- ps ]
        pos' = foldr union mempty $ fmap fst f'
        m'   = foldr merge (Base Map.empty) $ fmap snd f'
    in  follow pos' m' steps

walk :: Coord -> Direction -> Coord
walk (x, y) N = (x, y + 1)
walk (x, y) E = (x + 1, y)
walk (x, y) S = (x, y - 1)
walk (x, y) W = (x - 1, y)


data Explored = Explored Base (Map Coord Int)

instance Show Explored where

  show (Explored b e) =
    let
      Base m                       = b
      ((minx, miny), (maxx, maxy)) = mapSize . Map.keys $ m

      eof x y = if door x y E b then "  " else "# "
      nof x y = if door x y N b then "    " else "####"

      s x y = maybe "  " (printf "%2d") $ e !? (x, y)

      at y = "# "
        ++ concat [ printf "%s %s" (s x y) (eof x y) | x <- [minx .. maxx] ]
      above y = "#" ++ intercalate "#" [ nof x y | x <- [minx .. maxx] ] ++ "#"
      bottom = replicate (5 * (1 + maxx - minx) + 1) '#'
    in
      intercalate "\n"
                  [ above y ++ "\n" ++ at y | y <- [maxy, maxy - 1 .. miny] ]
      ++ "\n"
      ++ bottom


explore :: Base -> Explored
explore b = Explored b $ explore' Map.empty [((0, 0), 0)]
 where
  explore' :: Map Coord Int -> [(Coord, Int)] -> Map Coord Int
  explore' m []                             = m
  explore' m ((p, _) : ps) | Map.member p m = explore' m ps
  explore' m ((p, l) : ps) =
    let m' = Map.insert p l m
        ps' =
            nubBy ((==) `on` fst)
              . sortBy (compare `on` snd)
              . (<> ps)
              . fmap (, l + 1)
              . uncurry neighbors p
              $ b
    in  explore' m' ps'
