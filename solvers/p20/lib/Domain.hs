module Domain where

import           Data.List (intercalate)
import qualified Data.Map as Map

import Debug.Trace

data Direction = N | E | S | W
        deriving (Show, Eq, Ord)

data Step = Direction Direction
          | Fork [Path]
        deriving (Show, Eq)

data Path = Empty
          | Step Step
          | Sequence Step Path
        deriving (Show, Eq)

(<:>) :: Step -> Path -> Path
s <:> p = Sequence s p


showAllOptions :: [[Direction]] -> String
showAllOptions = (++) "\n" . intercalate "\n" . fmap (intercalate "" . fmap show)

followAll :: [Direction] -> Path -> [[Direction]]
followAll prev Empty                = traceShowId [prev]
followAll prev (Step (Direction d)) = [prev <> [d]]
followAll prev (Sequence s p)       =
        let variants = followAll prev (Step s)
            continuation variant = followAll variant p
        in  variants >>= continuation
followAll prev (Step (Fork ps))     = ps >>= followAll prev

lengthWithoutLoops :: [Direction] -> Int
lengthWithoutLoops = length' (Map.insert (0,0) 0 Map.empty) (0::Int,0::Int)
    where
        length' seen here []    = Map.findWithDefault 0 here seen
        length' seen here (d:p) =
            let here' = walk here d
                l     = Map.findWithDefault 0     here  seen
                l'    = Map.findWithDefault (l+1) here' seen
                seen' = Map.insert here' l' seen
             in length' seen' here' p

        walk (x,y) N = (x,y+1)
        walk (x,y) E = (x+1,y)
        walk (x,y) S = (x,y-1)
        walk (x,y) W = (x-1,y)

-- followLongest :: [Direction] -> Int -> Path -> [([Direction], Int)]
-- followLongest prev l Empty                = [(prev, l)]
-- followLongest prev l (Step (Direction d)) = [(d : prev, l + 1)]
-- followLongest prev l (Step (Fork ps))     = undefined
-- followLongest prev l (Sequence s p)       = followLongest prev l s <> followLongest prev l p
