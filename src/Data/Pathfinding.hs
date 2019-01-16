module Data.Pathfinding where

-- http://hackage.haskell.org/package/PSQueue-1.1.0.1/docs/Data-PSQueue.html
import Data.PSQueue (PSQ, Binding((:->)))
import qualified Data.PSQueue as Q
import Data.Set (Set)
import qualified Data.Set as Set

shortestPathReadingOrder :: Ord a => ((a,a) -> Bool) -> ((a,a) -> [(a,a)]) -> (a,a) -> [(a,a)] -> Maybe [(a,a)]
shortestPathReadingOrder walkable nexts source targets =
  let
    prio path = (length path, path)
    swap (x,y) = (y,x)
    len (l,_) = l
  in do
    path <- shortestPath walkable nexts prio len (swap source) (Set.fromList . fmap swap $ targets)
    return $ fmap swap path

shortestPath :: (Ord a, Ord p) => (a -> Bool) -> (a -> [a]) -> ([a] -> p) -> (p -> Int) -> a -> Set a -> Maybe [a]
shortestPath walkable nexts prioFor len source targets = _shortestPath Set.empty (Q.singleton [source] (prioFor [source]))
  where
    _shortestPath seen q
      | Q.null q = Nothing
      | otherwise = case Q.minView q of
        Nothing -> Nothing
        Just ((here:path) :-> p, q')
          | here `Set.member` targets -> Just . reverse $ here:path
          | here `Set.member` seen -> _shortestPath seen q'
          | otherwise ->
            let
              l = 1 + len p
              n = fmap (\x -> (x:here:path)) . filter walkable . nexts $ here
              q'' = _insertMany prioFor q' n
              seen' = Set.insert here seen
            in
              _shortestPath seen' q''

    _insertMany :: (Ord p, Ord a) => ([a] -> p) -> PSQ [a] p -> [[a]] -> PSQ [a] p
    _insertMany prio q [] = q
    _insertMany prio q (k:ks) =
      let q' = Q.insert k (prio . reverse $ k) q
      in _insertMany prio q' ks
