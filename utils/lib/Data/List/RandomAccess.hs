{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.List.RandomAccess
  ( RList
  , size
  , cons
  , head
  , tail
  , lookup
  , update
  , empty
  , isEmpty
  , fromList
  , toList
  , beginsWith
  , endsWith
  )
where
  -- This is based directly on Purely Functional Data Structures by Chris Okasaki
  -- CMU-CS-96-177
  -- https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf, p 77f

import           Data.Foldable
import           Prelude                 hiding ( head
                                                , length
                                                , lookup
                                                , tail
                                                )

data Tree a
  = Leaf a
  | Node Int (Tree a) (Tree a)
  deriving (Show, Eq)

data Digit a
  = Zero
  | One (Tree a)
  deriving (Show, Eq)

data RList a =
  RList [Digit a]
  deriving (Eq)

empty :: RList a
empty = RList []

isEmpty :: RList a -> Bool
isEmpty (RList as) = null as

beginsWith :: Eq a => [a] -> RList a -> Bool
beginsWith []       _  = True
beginsWith (a : as) rs = a == head rs && beginsWith as (tail rs)

endsWith :: Eq a => [a] -> RList a -> Bool
endsWith as rs = all match . zip [0 ..] $ as
 where
  la = length as
  lr = length rs
  match (i, a) = lookup (lr - 1 - la + i) rs == a

fromList :: [a] -> RList a
fromList []       = empty
fromList (x : xs) = cons x $ fromList xs

size :: Tree a -> Int
size (Leaf _    ) = 1
size (Node s _ _) = s

cons :: a -> RList a -> RList a
cons t (RList ts) = RList $ insertTree (Leaf t) ts
 where
  insertTree :: Tree a -> [Digit a] -> [Digit a]
  insertTree t' []              = [One t']
  insertTree t' (Zero    : ts') = One t' : ts'
  insertTree t' (One t'' : ts') = Zero : insertTree (link t' t'') ts'
   where
    link :: Tree a -> Tree a -> Tree a
    link t1 t2 = Node (size t1 + size t2) t1 t2

head :: RList a -> a
head (RList ts) = let (Leaf x, _) = borrowTree ts in x

tail :: RList a -> RList a
tail (RList ts) = let (_, ts') = borrowTree ts in RList ts'

borrowTree :: [Digit a] -> (Tree a, [Digit a])
borrowTree []           = error ("empty list")
borrowTree [One t     ] = (t, [])
borrowTree (One t : ts) = (t, Zero : ts)
borrowTree (Zero : ts) =
  let (Node _ t1 t2, tss) = borrowTree ts in (t1, One t2 : tss)

lookup :: Int -> RList a -> a
lookup i (RList []         ) = error $ "index out of bounds: " ++ show i
lookup i (RList (Zero : ts)) = lookup i (RList ts)
lookup i (RList (One t : ts)) | i < size t = lookupTree i t
                              | otherwise  = lookup (i - size t) (RList ts)
 where
  lookupTree 0  (Leaf x) = x
  lookupTree i' (Leaf _) = error $ "index out of bounds: " ++ show i'
  lookupTree i' (Node w t1 t2) | i' < w `div` 2 = lookupTree i' t1
                               | otherwise      = lookupTree (i' - w `div` 2) t2

update :: (a -> a) -> Int -> RList a -> RList a
update _ _ (RList []) = (RList [])
update f i (RList (Zero : ts)) =
  let RList updated = update f i (RList ts) in RList $ Zero : updated
update f i (RList (One t : ts))
  | i < size t
  = RList $ One (updateTree i t) : ts
  | otherwise
  = let RList rest = update f (i - size t) (RList ts) in RList $ One t : rest
 where
  updateTree 0 (Leaf x) = Leaf (f x)
  updateTree _ (Leaf x) = Leaf x
  updateTree i' (Node w t1 t2)
    | i' < w `div` 2 = Node w (updateTree i' t1) t2
    | otherwise      = Node w t1 (updateTree (i' - w `div` 2) t2)

instance Foldable Tree where
  foldMap f (Leaf a) = f a
  foldMap f (Node _ t1 t2) =
    let l = foldMap f t1
        r = foldMap f t2
    in  l <> r

instance Foldable RList where
  foldMap _ (RList []          ) = mempty
  foldMap f (RList (Zero  : ts)) = foldMap f $ RList ts
  foldMap f (RList (One t : ts)) = foldMap f t <> (foldMap f $ RList ts)
  length (RList []          ) = 0
  length (RList (Zero  : ts)) = length (RList ts)
  length (RList (One t : ts)) = size t + length (RList ts)

instance Functor Tree where
  fmap f (Leaf a      ) = Leaf (f a)
  fmap f (Node s t1 t2) = Node s (fmap f t1) (fmap f t2)

instance Functor RList where
  fmap _ (RList []         ) = empty
  fmap f (RList (Zero : ts)) = RList (Zero : fmap f' ts)
   where
    f' Zero    = Zero
    f' (One t) = One (fmap f t)
  fmap f (RList (One t : ts)) = RList $ (One (fmap f t) : fmap f' ts)
   where
    f' Zero     = Zero
    f' (One t') = One (fmap f t')

instance (Show a) => Show (RList a) where
  show l = "fromList " ++ show (toList l)
