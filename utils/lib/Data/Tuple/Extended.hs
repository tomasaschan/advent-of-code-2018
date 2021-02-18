module Data.Tuple.Extended
    ( fst3
    , snd3
    , thd3
    , first3
    , second3
    , third3
    , uncurry3
    , swap3ab
    , swap3ac
    , swap3bc
    , module Data.Tuple
    ) where

import           Data.Tuple

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z

first3 :: (a -> a') -> (a, b, c) -> (a', b, c)
first3 f (x, y, z) = (f x, y, z)

second3 :: (b -> b') -> (a, b, c) -> (a, b', c)
second3 f (x, y, z) = (x, f y, z)

third3 :: (c -> c') -> (a, b, c) -> (a, b, c')
third3 f (x, y, z) = (x, y, f z)

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f = let f' (a, b, c) = f a b c in f'

swap3ab :: (a, b, c) -> (b, a, c)
swap3ab (x, y, z) = (y, x, z)

swap3bc :: (a, b, c) -> (a, c, b)
swap3bc (x, y, z) = (x, z, y)

swap3ac :: (a, b, c) -> (c, b, a)
swap3ac (x, y, z) = (z, y, x)
