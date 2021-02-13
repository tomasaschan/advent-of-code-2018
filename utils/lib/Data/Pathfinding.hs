module Data.Pathfinding where

-- http://hackage.haskell.org/package/PSQueue-1.1.0.1/docs/Data-PSQueue.html
import           Data.PSQueue                   ( Binding((:->))
                                                , PSQ
                                                )
import qualified Data.PSQueue                  as Q
import           Data.Sequence                  ( Seq((:<|), Empty) )
import qualified Data.Sequence                 as Seq
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

bfsMapTraversal
  :: (Ord a, Ord p)
  => (a -> Bool)
  -> (a -> Seq a)
  -> (Seq a -> p)
  -> a
  -> Set a
  -> Maybe (Seq a)
bfsMapTraversal walkable neigbors prio start ends = _bfs
  Set.empty
  (Q.singleton (Seq.singleton start) (prio . Seq.singleton $ start))
 where
  _bfs seen q = case Q.minView q of
    Nothing               -> Nothing
    Just (Empty :-> _, _) -> Nothing
    Just ((here :<| path) :-> _, q')
      | here `Set.member` ends
      -> Just . Seq.reverse $ here :<| path
      | here `Set.member` seen
      -> _bfs seen q'
      | otherwise
      -> let append x = x :<| (here :<| path)
             n     = fmap append . Seq.filter walkable . neigbors $ here
             q''   = _insertMany prio q' n
             seen' = Set.insert here seen
         in  _bfs seen' q''
  _insertMany
    :: (Ord p, Ord a)
    => (Seq a -> p)
    -> PSQ (Seq a) p
    -> Seq (Seq a)
    -> PSQ (Seq a) p
  _insertMany _ q Empty = q
  _insertMany p q (path :<| paths) =
    let q' = Q.insert path (p path) q in _insertMany p q' paths
