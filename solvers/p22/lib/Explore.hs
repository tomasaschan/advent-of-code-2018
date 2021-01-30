module Explore (findRudolph) where

import           Debug.Trace
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.PSQueue (Binding ((:->)), PSQ)
import qualified Data.PSQueue as Q

import Cave


data Tool = Torch | ClimbingGear | Neither deriving (Eq, Ord, Show)
newtype Time = Time Int deriving (Eq, Ord)

instance Show Time where
    show (Time t) = show t ++ " minutes"

data Direction = N | S | E | W
data Action = Move Direction | SwitchOnce | SwitchTwice
allMoves :: [Action]
allMoves = [Move N, Move S, Move E, Move W, SwitchOnce, SwitchTwice]

data State = State Region Tool deriving (Eq, Ord, Show)

move :: Region -> Direction -> Region
move (x,y) N = (x,y-1)
move (x,y) S = (x,y+1)
move (x,y) W = (x-1,y)
move (x,y) E = (x+1,y)

switch :: Tool -> Tool
switch Torch        = ClimbingGear
switch ClimbingGear = Neither
switch Neither      = Torch

usable :: Type -> Tool -> Bool
usable Wet    Torch        = False
usable Wet    ClimbingGear = True
usable Wet    Neither      = True

usable Narrow Torch        = True
usable Narrow ClimbingGear = False
usable Narrow Neither      = True

usable Rocky  Torch        = True
usable Rocky  ClimbingGear = True
usable Rocky  Neither      = False

(++>) :: Time -> Int -> Time
(Time t) ++> t' = Time (t + t')

newtype Distance = Distance Int deriving (Eq, Show, Ord)

findRudolph :: Depth -> Target -> Time
findRudolph depth (Target (tx,ty)) =
    let bfs :: Map State Time -> PSQ State Time -> (Int,Int) -> Time
        bfs seen q (xmax,ymax) =
            case Q.minView q of
                Nothing             -> Time (-1)
                Just (s :-> t , _ )
                    | s == goal     -> traceShow (s, goal, depth) t
                Just (s :-> t, q') ->
                    let seen' = storeMin s t seen

                        sts' = filter isFastest . filter isValid . fmap (act s) $ allMoves

                        q'' = insertMany sts' q'

                        State (x,y) _ = s
                        (xmax',ymax') = if x > xmax || y > ymax
                                        then traceShow (max x xmax, max y ymax, t) (max x xmax, max y ymax)
                                        else (xmax,ymax)

                        act (State r tl) (Move d)    = State (move r d) tl                   :-> t ++> 1
                        act (State r tl) SwitchOnce  = State r          (switch tl)          :-> t ++> 7
                        act (State r tl) SwitchTwice = State r          (switch $ switch tl) :-> t ++> 7

                        isValid ((State (x',y') _) :-> _) | notExplored (x',y') = False
                        isValid ((State r      t') :-> _)                       = usable (assess $ erosion !!! r) t'

                        isFastest (s' :-> t') = maybe True (t' <) $ Map.lookup s' seen

                    in q'' `seq` seen' `seq` xmax' `seq` ymax' `seq` bfs seen' q'' (xmax',ymax')

        insertMany []             = id
        insertMany ((s:->p):rest) = insertMany rest . Q.insert s p

        storeMin s t seen = Map.alter f s seen
            where f Nothing   = Just t
                  f (Just t') = Just $ min t t'

        goal    = State (tx,ty) Torch
        erosion = explore (10*tx,5*ty) $ emptyErosion depth (Target (tx,ty))

        notExplored (x,y) = let ErosionMap _ _ e = erosion
                             in (x,y) `Map.notMember` e
        q0      = Q.singleton (State (0,0) Torch) (Time 0)



    in bfs Map.empty q0 (0,0)
