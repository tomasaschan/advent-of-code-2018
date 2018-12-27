module Solvers.Dec11 where

  import Data.Map (Map, (!), insert, empty)
  import Text.Printf
  import Data.Foldable (maximumBy)
  import Data.Function (on)

  -- import Debug.Trace
  trace :: String -> a -> a
  trace _ x = x

  type X = Int
  type Y = Int
  type SerialNumber = Int
  type Coord = (X,Y)
  type Level = Int
  type Size = Int
  type Grid = Map Coord Level
  type SummedAreaTable = Map Coord Level
  type PowerFn = Coord -> Level
  type CellGenerator = Size -> [Coord]
  type LargeCellIdentifier = (X,Y,Size)

  solveA :: [String] -> String
  solveA = show . solve . read . head
    where
      solve sn = answer
        where
          sat = summedAreaTable 300 $ power sn
          cells = cellsOfSize 300 3
          (x,y,_) = superCellWithHighestPower sat cells
          answer = (x,y)

  solveB :: [String] -> String
  solveB = show . solve . read . head
    where
      solve sn = answer
        where
          sat = summedAreaTable 300 $ power sn
          cells = concatMap (cellsOfSize 300) [1..300]
          (x,y,s) = superCellWithHighestPower sat cells
          answer = (x,y,s)

  power :: SerialNumber -> PowerFn
  power serialNumber (x,y) = (div p' 100 `mod` 10) - 5
    where
      rackId = x + 10
      p = rackId * y + serialNumber
      p' = p * rackId

  summedAreaTable :: Size -> PowerFn -> SummedAreaTable
  summedAreaTable n p = _extend 1 1 empty
    where
      at x y prev
        | x == 1 && y == 1 = p (1,1)
        | x == 1           = p (1,y) + prev ! (1,y-1)
        | y == 1           = p (x,1) + prev ! (x-1,1)
        | otherwise        = p (x,y) + prev ! (x-1,y) + prev ! (x,y-1) - prev ! (x-1,y-1)

      _extend x y sofar
        | x == n && y == n = insert (n,n) (at x y sofar) sofar
        | x == n = _extend 1 (y+1) next
        | otherwise = _extend (x+1) y next
          where
            this = at x y sofar
            next = insert (x,y) this sofar

  areaPowerLookup :: SummedAreaTable -> LargeCellIdentifier -> Level
  areaPowerLookup sat (x,y,s) = trace (printf "looking up power at (%d,%d) with size %d. a=%d, b=%d, c=%d, d=%d, p=%d" x y s a b c d (a+b-c-d)) (a + b - c - d)
    where
      p x' y'
        | x' < 1 || y' < 1 = 0
        | otherwise = sat ! (x',y')
      xm = x-1
      ym = y-1
      xp = x+s-1
      yp = y+s-1
      a = trace (printf "(xm,ym) = (%d,%d)" xm ym) (p xm ym)
      b = trace (printf "(xp,yp) = (%d,%d)" xp yp) (p xp yp)
      c = p xp ym
      d = p xm yp

  cellsOfSize :: Size -> Size -> [(LargeCellIdentifier)]
  cellsOfSize n s = [ (x,y,s) | x <- [1..n-s+1], y <- [1..n-s+1] ]

  superCellWithHighestPower :: SummedAreaTable -> [(LargeCellIdentifier)] -> LargeCellIdentifier
  superCellWithHighestPower sat = maximumBy (compare `on` areaPowerLookup sat)
