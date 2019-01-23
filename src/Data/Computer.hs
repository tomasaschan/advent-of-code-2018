module Data.Computer where

  import Prelude hiding (lookup)
  import Data.Bits
  import Data.IntMap hiding (size)

  data Reftype = Register | Value
  data Op =
      AddR | AddI
    | MulR | MulI
    | BanR | BanI
    | BorR | BorI
    | SetR | SetI
    | GtIR | GtRI | GtRR
    | EqIR | EqRI | EqRR
    deriving (Show, Eq, Enum, Bounded)

  apply :: Op -> Int -> Int -> Int -> Registry -> Registry
  apply AddR x y z r = setValue z (valueAt x r  +  valueAt y r) r
  apply AddI x y z r = setValue z (valueAt x r  +          y  ) r
  apply MulR x y z r = setValue z (valueAt x r  *  valueAt y r) r
  apply MulI x y z r = setValue z (valueAt x r  *          y  ) r
  apply BanR x y z r = setValue z (valueAt x r .&. valueAt y r) r
  apply BanI x y z r = setValue z (valueAt x r .&.         y  ) r
  apply BorR x y z r = setValue z (valueAt x r .|. valueAt y r) r
  apply BorI x y z r = setValue z (valueAt x r .|.         y  ) r
  apply SetR x _ z r = setValue z (valueAt x r) r
  apply SetI x _ z r = setValue z          x    r
  apply GtIR x y z r
    | x           > valueAt y r = setValue z 1 r
    | otherwise                 = setValue z 0 r
  apply GtRI x y z r
    | valueAt x r > y           = setValue z 1 r
    | otherwise                 = setValue z 0 r
  apply GtRR x y z r
    | valueAt x r > valueAt y r = setValue z 1 r
    | otherwise                 = setValue z 0 r
  apply EqIR x y z r
    | x           == valueAt y r = setValue z 1 r
    | otherwise                  = setValue z 0 r
  apply EqRI x y z r
    | valueAt x r == y           = setValue z 1 r
    | otherwise                  = setValue z 0 r
  apply EqRR x y z r
    | valueAt x r == valueAt y r = setValue z 1 r
    | otherwise                  = setValue z 0 r

  data Registry = Registry (IntMap Int) deriving (Show, Eq)

  initial :: Int -> Registry
  initial size = Registry . Data.IntMap.fromList . fmap (flip (,) 0) $ [0..size-1]

  fromList :: [Int] -> Registry
  fromList = Registry . Data.IntMap.fromList . zip [0..]

  valueAt :: Int -> Registry -> Int
  valueAt k (Registry m) =
    case lookup k m of
      Just v -> v
      Nothing -> -1

  setValue :: Int -> Int -> Registry -> Registry
  setValue k v (Registry m) = Registry $ adjust (const v) k m
