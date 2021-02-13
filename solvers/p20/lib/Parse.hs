module Parse where

import           Domain

path :: String -> [Step]
path ('^' : p) = path p
path "$"       = []

path ('N' : p) = Direction N : path p
path ('E' : p) = Direction E : path p
path ('S' : p) = Direction S : path p
path ('W' : p) = Direction W : path p

path ('(' : p) = let (f, p') = fork p in (Fork f : path p')

path (c   : p) = error $ "Unexpected: " ++ show c ++ " at " ++ show p
path []        = error "Unexpected EOF"

fork :: String -> ([[Step]], String)
fork = fork' [] []
 where
  fork' :: [[Step]] -> [Step] -> String -> ([[Step]], String)
  fork' choices current (')' : p) = (reverse current : choices, p)
  fork' choices current ('|' : p) = fork' (reverse current : choices) [] p
  fork' choices current ('N' : p) = fork' choices (Direction N : current) p
  fork' choices current ('E' : p) = fork' choices (Direction E : current) p
  fork' choices current ('S' : p) = fork' choices (Direction S : current) p
  fork' choices current ('W' : p) = fork' choices (Direction W : current) p
  fork' choices current ('(' : p) =
    let (f, p') = fork p in fork' choices (Fork f : current) p'

  fork' _ _ (c : _) = error $ "Unexpected: " ++ show c
  fork' _ _ []      = error "Unexpected EOF"
