module Solver where

import           Data.List.Extended
import           Data.List.Split
import           Data.Maybe

solveA :: [String] -> String
solveA = show . sumMeta . parse . head

solveB :: [String] -> String
solveB = show . nodeValue . parse . head

ints :: String -> [Int]
ints = map read . splitOn " "

parse :: String -> NodeInfo
parse = fst . readNode . ints

data Header =
  Header
    { childCount :: Int
    , metaCount  :: Int
    }
  deriving (Eq, Show)

data NodeInfo =
  NodeInfo
    { header   :: Header
    , children :: [NodeInfo]
    , meta     :: [Int]
    }
  deriving (Eq, Show)

readNode :: [Int] -> (NodeInfo, [Int])
readNode xs = (node, rest)
 where
  (hdr, xss  ) = readHeader xs
  (nds, xsss ) = readNodes (childCount hdr) xss
  (mta, xssss) = readMeta (metaCount hdr) xsss
  node         = NodeInfo { header = hdr, children = nds, meta = mta }
  rest         = xssss

readNodes :: Int -> [Int] -> ([NodeInfo], [Int])
readNodes 0     xs = ([], xs)
readNodes count xs = (nodes, rest)
 where
  (first , rest') = readNode xs
  (rest'', rest ) = readNodes (count - 1) rest'
  nodes           = first : rest''

readHeader :: [Int] -> (Header, [Int])
readHeader (a : b : rest) = (Header { childCount = a, metaCount = b }, rest)
readHeader _ = error "not enough elements in the list to read a header"

readMeta :: Int -> [Int] -> ([Int], [Int])
readMeta count xs = (take count xs, drop count xs)

sumMeta :: NodeInfo -> Int
sumMeta (NodeInfo { children = c, meta = m }) = sum (m ++ map sumMeta c)

nodeValue :: NodeInfo -> Int
nodeValue node
  | hasChildren node = sum $ map nodeValue $ referencedNodes (children node)
                                                             (meta node)
  | otherwise = sum $ meta node

hasChildren :: NodeInfo -> Bool
hasChildren = (<) 0 . length . children

getOneIndexedMaybe :: [a] -> Int -> Maybe a
getOneIndexedMaybe as i = as !? (i - 1)

referencedNodes :: [NodeInfo] -> [Int] -> [NodeInfo]
referencedNodes c = catMaybes . map (getOneIndexedMaybe c)
