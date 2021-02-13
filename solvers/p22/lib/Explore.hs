module Explore
    ( findRudolph
    ) where

import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.PQueue.Prio.Min    hiding ( filter )

import           Cave


data Tool = Torch | ClimbingGear | Neither deriving (Eq, Ord, Show)
newtype Time = Time Int deriving (Eq, Ord)

instance Show Time where
    show (Time t) = show t ++ " minutes"

data Direction = N | S | E | W
data Rotation = CW | CCW

allMoves :: [(Time, State) -> (Time, State)]
allMoves = [move N, move S, move E, move W, switch CW, switch CCW]

data State = State Region Tool
    deriving (Eq, Ord, Show)

move :: Direction -> (Time, State) -> (Time, State)
move N (t, State (x, y) tl) = (t ++> 1, State (x, y - 1) tl)
move S (t, State (x, y) tl) = (t ++> 1, State (x, y + 1) tl)
move W (t, State (x, y) tl) = (t ++> 1, State (x - 1, y) tl)
move E (t, State (x, y) tl) = (t ++> 1, State (x + 1, y) tl)

switch :: Rotation -> (Time, State) -> (Time, State)
switch CW  (t, State r Torch       ) = (t ++> 7, State r ClimbingGear)
switch CCW (t, State r Torch       ) = (t ++> 7, State r Neither)

switch CW  (t, State r ClimbingGear) = (t ++> 7, State r Neither)
switch CCW (t, State r ClimbingGear) = (t ++> 7, State r Torch)

switch CW  (t, State r Neither     ) = (t ++> 7, State r Torch)
switch CCW (t, State r Neither     ) = (t ++> 7, State r ClimbingGear)

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

isValid :: ErosionMap -> (Time, State) -> Bool
isValid _ (_, State (x, y) _) | x < 0 || y < 0 || x > 35 = False
isValid em (_, State r tool) = usable (typeAt em r) tool

(++>) :: Time -> Int -> Time
(Time t) ++> t' = Time (t + t')

newtype Distance = Distance Int deriving (Eq, Show, Ord)

newtype Seen = Seen (Map State Time)

see :: Seen -> Time -> State -> Seen
see (Seen ss) t s = Seen $ Map.insert s t ss

timeTo :: Seen -> State -> Maybe Time
timeTo (Seen ss) s = Map.lookup s ss

isFastest :: Seen -> (Time, State) -> Bool
isFastest seen (t, s) = maybe True (> t) $ timeTo seen s

findRudolph :: Depth -> Target -> Time
findRudolph depth target =
    let q0       = singleton (Time 0) (State (0, 0) Torch)
        Target r = target
        e0       = mapOutCave depth target
        goal     = State r Torch
        seen0    = Seen mempty

        enqueueAll []            q = q
        enqueueAll ((t, s) : nx) q = enqueueAll nx $ insert t s q

        bfs e q seen = case deleteFindMin q of
            (_, q') | size q' > 10000 -> error "queue grew too big"
            ((t, s), _) | s == goal -> t
            ((t, s), q') | not $ isFastest seen (t, s) -> bfs e q' seen
            ((t, s), q') ->
                let State (x, y) _ = s

                    nexts =
                        filter (isFastest seen) $ filter (isValid e') $ fmap
                            (\m -> m (t, s))
                            allMoves

                    seen'  = see seen t s
                    queue' = enqueueAll nexts q'

                    e'     = explore (x + 1, y) $ explore (x, y + 1) e
                in  bfs e' queue' seen'
    in  bfs e0 q0 seen0
