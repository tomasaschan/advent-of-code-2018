module Data.Pathfinding where

-- http://hackage.haskell.org/package/PSQueue-1.1.0.1/docs/Data-PSQueue.html
import Data.PSQueue (PSQ, Binding((:->)))
import qualified Data.PSQueue as Q
import Data.Set (Set)
import Data.Tuple (swap)
import qualified Data.Set as Set

shortestPathReadingOrder :: Ord a => ((a,a) -> Bool) -> ((a,a) -> [(a,a)]) -> (a,a) -> [(a,a)] -> Maybe [(a,a)]
shortestPathReadingOrder walkable nexts source targets =
  let
    -- we want to prioritize first by length, then by reading order on the first differing position
    -- tuples and lists are sorted lexically, so two lists of (y,x) pairs will sort according to
    -- the first one that differs, and (y,x) sorts in reading order
    prio path = (l, t, p)
      where
        l = length path
        t = swap . head $ path
        p = fmap swap . reverse $ path
    len (l,_,_) = l
  in shortestPath walkable nexts prio len source targets

shortestPath :: (Ord a, Ord p) => (a -> Bool) -> (a -> [a]) -> ([a] -> p) -> (p -> Int) -> a -> [a] -> Maybe [a]
shortestPath walkable nexts prioFor len source targets = _shortestPath Set.empty (Q.singleton [source] (prioFor [source]))
  where
    _shortestPath seen q =
        case Q.minView q of
          -- there are new points left to visit; if we haven't already found a path, there is none
          Nothing -> Nothing
          -- there's at least one uninvestigated path left; let's take a look!
          -- here: the new position we're investigating
          -- path: the path we took to get here
          Just ((here:path) :-> p, q')
            -- the path leads to one of the targets - we're done!
            | here `elem` targets -> Just . reverse $ here:path
            -- we've already been here; just ignore it and keep looking elsewhere
            | here `Set.member` seen -> _shortestPath seen q'
            -- we've moved to a new square that's not one of the targets
            | otherwise ->
              let
                -- accumulate the path backwards (previous steps are later in the list)
                -- we'll have to reverse the path to get it as a list of points from source to target
                append x = x:here:path
                -- each element of n is a potential path, with an un-investigated position at the head
                -- and the path to get back to the start (i.e. the path we took to here, but backwards)
                -- in the tail
                n = fmap append . filter walkable . nexts $ here

                -- queue all potential next paths. they will sort themselves according to the prioFor
                -- function, which takes care of both length and tie-breaking
                q'' = _insertMany prioFor q' n
                seen' = Set.insert here seen
              in
                -- recursively keep looking
                _shortestPath seen' q''

    _insertMany :: (Ord p, Ord a) => ([a] -> p) -> PSQ [a] p -> [[a]] -> PSQ [a] p
    _insertMany prio q [] = q
    _insertMany prio q (path:paths) =
      let
        -- insert the first one
        q' = Q.insert path (prio path) q
      in
        -- insert the others
        _insertMany prio q' paths
