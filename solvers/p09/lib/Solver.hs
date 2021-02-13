{-# LANGUAGE FlexibleInstances #-}

module Solver where

import qualified Parse

import           Data.Foldable                  ( toList )
import qualified Data.Foldable                 as F
                                                ( foldr )
import           Data.List                      ( intercalate )
import           Data.Sequence                  ( adjust
                                                , fromList
                                                )

newtype MarbleList a =
  ML ([a], [a])
  deriving (Eq, Show)

instance Foldable MarbleList where
  foldr f x (ML (before, after)) = F.foldr f x (reverse before ++ after)

empty :: MarbleList a
empty = ML ([], [])

right :: Int -> MarbleList a -> MarbleList a
right 1 (ML ([]  , []       )) = ML ([], [])
right 1 (ML (prev, x : after)) = ML (x : prev, after)
right 1 (ML (prev, []       )) = right 1 $ ML ([], reverse prev)
right n ml                     = right (n - 1) (right 1 ml)

left :: Int -> MarbleList a -> MarbleList a
left 1 (ML ([]      , []   )) = ML ([], [])
left 1 (ML (x : prev, after)) = ML (prev, x : after)
left 1 (ML ([]      , after)) = left 1 $ ML (reverse after, [])
left n ml                     = left (n - 1) (left 1 ml)

insert :: a -> MarbleList a -> MarbleList a
insert x (ML (prev, after)) = ML (prev, x : after)

remove :: MarbleList a -> (MarbleList a, a)
remove (ML (prev, x : after)) = (ML (prev, after), x)
remove (ML (prev, []       )) = remove $ (ML ([], reverse prev))

data State =
  State
    { marbles    :: MarbleList Int
    , toBePlaced :: Int
    , toPlay     :: Int
    , scores     :: [Integer]
    , lastMarble :: Int
    }
  deriving (Show, Eq)

initial :: (Int, Int) -> State
initial (ps, lm) = State { marbles    = insert 0 $ empty
                         , toBePlaced = 1
                         , toPlay     = 0
                         , scores     = replicate ps 0
                         , lastMarble = lm
                         }

nextPlayer :: State -> Int
nextPlayer s = flip (mod) (players s) . succ . toPlay $ s
 where
  players :: State -> Int
  players = length . scores

place :: State -> State
place s = s { marbles    = insert (toBePlaced s) . right 2 . marbles $ s
            , toBePlaced = succ . toBePlaced $ s
            , toPlay     = nextPlayer s
            }

score :: State -> State
score s = s { marbles    = marbles'
            , scores     = scores'
            , toPlay     = nextPlayer s
            , toBePlaced = succ . toBePlaced $ s
            }
 where
  (marbles', grabbed) = remove . left 7 . marbles $ s
  points              = toInteger $ toBePlaced s + grabbed
  scores'             = adjustList ((+) points) (toPlay s) (scores s)
  adjustList f i = toList . adjust f i . fromList

play :: State -> State
play s | toBePlaced s `mod` 23 == 0 = score s
       | otherwise                  = place s

playGame :: State -> State
playGame = head . filter gameOver . iterate play

gameOver :: State -> Bool
gameOver s = toBePlaced s > lastMarble s

solve :: Int -> String -> String
solve n =
  show
    . maximum
    . scores
    . head
    . filter gameOver
    . iterate play
    . scale
    . initial
    . parse
  where scale s = s { lastMarble = n * lastMarble s }

solveA :: [String] -> String
solveA = intercalate "," . fmap (solve 1)

solveB :: [String] -> String
solveB = intercalate "," . fmap (solve 100)

parse :: String -> (Int, Int)
parse = _tuplify . _parse
 where
  _parse x = extracted
   where
    parsed =
      Parse.digits x
        >>= Parse.expect " players; last marble is worth "
        >>= Parse.digits
        >>= Parse.expect " points"
    extracted = Parse.extract (fmap read) parsed
  _tuplify [x, y] = (x, y)
  _tuplify o      = error ("unexpected parser output: " ++ show o)
