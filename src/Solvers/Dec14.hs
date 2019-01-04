module Solvers.Dec14 where

  import Prelude hiding (lookup)
  import Data.RandomAccessList
  import Data.Digits

  import Debug.Trace
  debug :: Show a => String -> a -> a
  debug lbl x = trace (lbl ++ ": " ++ show x) x

  type Recipies = RList Int
  recipe :: Int -> Recipies -> Int
  recipe i rs = lookup (length rs - 1 - i) rs

  data State = State { recipies :: Recipies, elf1 :: Int, elf2 :: Int } deriving (Show, Eq)

  initial :: State
  initial = State (fromList $ reverse [3,7]) 0 1

  solveA :: [String] -> String
  solveA = unwords . fmap (solve . read)
    where solve n = concat . fmap show . drop n . take' (n + 10) $ initial

  take' :: Int -> State -> [Int]
  take' n s
    | n <= length (recipies s) = Prelude.take n . reverse . toList . recipies $ s
    | otherwise = take' n . next $ s

  next :: State -> State
  next s =
    let
      recipies' = append (nextRecipies s) (recipies s)
      elf1' = nextPos recipies' . elf1 $ s
      elf2' = nextPos recipies' . elf2 $ s
    in s {
      recipies = recipies' `seq` recipies',
      elf1 = elf1' `seq` elf1',
      elf2 = elf2' `seq` elf2'
    }
    where
      append :: [Int] -> Recipies -> Recipies
      append [] rs = rs
      append (r:rs) rss = append rs (cons r rss)

      nextRecipies :: State -> [Int]
      nextRecipies s' =
        let
          rs = recipies s'
          i = elf1 s'
          j = elf2 s'
          x = recipe i rs
          y = recipe j rs
        in digits 10 (x + y)

      nextPos :: Recipies -> Int -> Int
      nextPos rs i =
        let
          r = recipe i rs
          l = length rs
        in
          _np (i + 1 + r) l
        where
          _np i' l'
            | i' < l' = i'
            | otherwise = _np (i'-l') l'
