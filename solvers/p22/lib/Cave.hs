{-# LANGUAGE FlexibleInstances #-}

module Cave where

import Data.Default.Class
import Data.Foldable.Extended
import Data.Map hiding (foldl)
import Data.List (intercalate)

type Coord = (Int,Int)

draw :: Show a => Map Coord a -> String
draw m =
    let
        ((xmin,ymin),(xmax,ymax)) = mapSize . keys $ m
        line y = concat [show $ m ! (x,y) | x <- [xmin..xmax]]
    in intercalate "\n" [line y | y <- [ymin..ymax]]

newtype Erosion = Erosion (Map Coord Int)
instance Default Erosion where
    def = Erosion Data.Map.empty

instance Show Erosion where
    show (Erosion e) = draw e

data Type = Rocky
          | Wet
          | Narrow
newtype Cave = Cave (Map Coord Type)

instance Default Cave where
    def = Cave Data.Map.empty

instance Show Type where
    show Rocky  = "."
    show Wet    = "="
    show Narrow = "|"
instance Show Cave where
    show (Cave c) = draw c



geologicIndex :: Map Coord Int -> Int -> Coord -> Coord -> Int
geologicIndex _    _ _      (0,0)                   = 0
geologicIndex _    _ target coord | target == coord = 0
geologicIndex _    _ _      (x,0)                   = x * 16807
geologicIndex _    _ _      (0,y)                   = y * 48271
geologicIndex cave _ _      (x,y)                   = (cave ! (x-1, y)) * (cave ! (x, y-1))

erosionLevel :: Map Coord Int -> Int -> Coord -> Coord -> Int
erosionLevel cave depth target coord = (depth + geologicIndex cave depth target coord) `mod` 20183

coordsTo :: Coord -> [Coord]
coordsTo (x,y) = coordsTo' (0,0)
    where
        coordsTo' (x',y') | x' == x && y' == y = [(x',y')]
        coordsTo' (x',y') |            y' == y =  (x',y') : coordsTo' (x'+1, 0   )
        coordsTo' (x',y')                      =  (x',y') : coordsTo' (x',   y'+1)


explore :: Int -> Coord -> Coord -> Erosion -> Erosion
explore _     _      coord (Erosion cave) | coord `member` cave = Erosion cave
explore depth target coord erosion                              = foldl folder erosion (coordsTo coord)
    where folder (Erosion e) c | c `member` e = Erosion e
          folder (Erosion e) c                = Erosion $ insert c (erosionLevel e depth target c) e

assess :: Erosion -> Cave
assess (Erosion e) = Cave $ toType . flip mod 3 <$> e
    where
        toType 0 = Rocky
        toType 1 = Wet
        toType 2 = Narrow
        toType _ = undefined


riskLevel :: Type -> Int
riskLevel Rocky  = 0
riskLevel Wet    = 1
riskLevel Narrow = 2
