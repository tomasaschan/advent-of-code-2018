{-# LANGUAGE FlexibleInstances #-}

module Cave where

import           Data.Foldable.Extended
import           Data.List                      ( intercalate )
import           Data.Map.Strict         hiding ( foldl )
import           Debug.Trace

type Region = (Int, Int)

newtype Depth = Depth Int deriving (Eq, Show)
newtype ErosionLevel = ErosionLevel Int deriving (Eq, Show)
newtype GeologicIndex = GeologicIndex Int deriving (Eq, Show)
newtype Target = Target Region deriving (Eq, Show)

(===) :: Target -> Region -> Bool
(Target t) === r = t == r

draw :: Show a => Map Region a -> String
draw m =
    let ((xmin, ymin), (xmax, ymax)) = mapSize . keys $ m
        line y = concat [ show $ m ! (x, y) | x <- [xmin .. xmax] ]
    in  intercalate "\n" [ line y | y <- [ymin .. ymax] ]

data ErosionMap = ErosionMap Depth Target (Map Region ErosionLevel)

mapOutCave :: Depth -> Target -> ErosionMap
mapOutCave depth (Target target) =
    explore target $ ErosionMap depth (Target target) mempty

instance Show ErosionMap where
    show (ErosionMap _ _ e) = draw e

data Type = Rocky
          | Wet
          | Narrow
newtype Cave = Cave (Map Region Type)

instance Show Type where
    show Rocky  = "."
    show Wet    = "="
    show Narrow = "|"
instance Show Cave where
    show (Cave c) = draw c

(***) :: ErosionLevel -> ErosionLevel -> GeologicIndex
(ErosionLevel a) *** (ErosionLevel b) = a `seq` b `seq` GeologicIndex (a * b)

(!!!) :: ErosionMap -> Region -> ErosionLevel
(ErosionMap _ _ m) !!! r = if r `member` m
    then m ! r
    else error $ show r ++ " has not been explored yet!"

geologicIndex :: ErosionMap -> Region -> GeologicIndex
geologicIndex _ (0, 0) = GeologicIndex 0
geologicIndex (ErosionMap _ target _) region | target === region =
    GeologicIndex 0
geologicIndex _ (x, 0) = GeologicIndex $ x * 16807
geologicIndex _ (0, y) = GeologicIndex $ y * 48271
geologicIndex erosionMap (x, y) =
    let a = erosionMap !!! (x - 1, y)
        b = erosionMap !!! (x, y - 1)
    in  a `seq` b `seq` a *** b

(+++) :: Depth -> GeologicIndex -> ErosionLevel
(Depth d) +++ (GeologicIndex i) = ErosionLevel ((d + i) `mod` 20183)

erosionLevel :: ErosionMap -> Region -> ErosionLevel
erosionLevel (ErosionMap depth target cave) coord =
    depth +++ geologicIndex (ErosionMap depth target cave) coord

coordsTo :: Region -> [Region]
coordsTo (x, y) = coordsTo' (0, 0)
  where
    coordsTo' (x', y') | x' == x && y' == y = [(x', y')]
    coordsTo' (x', y') | y' == y            = (x', y') : coordsTo' (x' + 1, 0)
    coordsTo' (x', y')                      = (x', y') : coordsTo' (x', y' + 1)

explore :: Region -> ErosionMap -> ErosionMap
explore (x, y) m | x < 0 || y < 0 = m
explore coord (ErosionMap d t cave) | coord `member` cave = ErosionMap d t cave
explore coord erosion             = foldl folder erosion (coordsTo coord)
  where
    folder (ErosionMap d t m) c | c `member` m = ErosionMap d t m
    folder (ErosionMap d t m) c =
        ErosionMap d t $ insert c (erosionLevel (ErosionMap d t m) c) m

assess :: ErosionLevel -> Type
assess (ErosionLevel e) = assess' $ e `mod` 3
  where
    assess' 0 = Rocky
    assess' 1 = Wet
    assess' 2 = Narrow
    assess' _ = undefined

assessAll :: ErosionMap -> Cave
assessAll (ErosionMap _ _ e) = Cave $ fmap assess e

riskLevel :: Type -> Int
riskLevel Rocky  = 0
riskLevel Wet    = 1
riskLevel Narrow = 2

typeAt :: ErosionMap -> Region -> Type
typeAt (ErosionMap _ _ cave) r | r `member` cave = assess $ cave ! r
typeAt em r = trace ("typeAt " ++ show r) $ typeAt (explore r em) r
