module Data.Either.Extended
  (
    module Data.Either,
    foldAll
  )
where

import Data.Either

foldAll :: [Either e a] -> Either e [a]
foldAll []   = Right []
foldAll ((Right x):xs) =
  case foldAll xs of
    Right xs' -> Right (x:xs')
    Left e -> Left e

