module Solvers.Dec7 where

  import Data.Hashable
  import Data.Graph.DGraph
  import Data.Graph.Types
  import Data.Sort
  import Data.List (elemIndex, intercalate)
  import Data.Maybe (fromJust)
  import Text.Printf

  import qualified Parse

  solveA :: [String] -> String
  solveA = finalA . filter isDoneA . iterate nextA . initialA
    where
      initialA = (,) "" . parse id
      isDoneA (_, g) = order g == 0
      nextA (work, graph) = (work ++ nextUp, removeVertex nextUp graph)
        where nextUp = head $ ready graph
      finalA = fst . head

  data BState = BState {
    now :: Int,
    inProgress :: [(String, Int)],
    dependencies :: DependencyTree (String, Int),
    workers :: Int
  } deriving (Eq)

  instance Show BState where
    show (BState { now = t, inProgress = work }) = printf "% 5d   %s" t (intercalate "," $ fmap fst work)

  now' :: (Int -> Int) -> BState -> BState
  now'          f (BState { now = t, inProgress = ip, dependencies = d, workers = n }) = BState { now = f t, inProgress = ip, dependencies = d, workers = n }
  inProgress' :: ([(String, Int)] -> [(String, Int)]) -> BState -> BState
  inProgress'   f (BState { now = t, inProgress = ip, dependencies = d, workers = n }) = BState { now = t, inProgress = f ip, dependencies = d, workers = n }
  dependencies' :: (DependencyTree (String, Int) -> DependencyTree (String, Int)) -> BState -> BState
  dependencies' f (BState { now = t, inProgress = ip, dependencies = d, workers = n }) = BState { now = t, inProgress = ip, dependencies = f d, workers = n }

  solveB :: Int -> Int -> [String] -> String
  solveB n extra = show . now . head . filter isDone . iterate next . initial n extra

  initial :: Int -> Int -> [String] -> BState
  initial n extra input = BState { now = 0, inProgress = [], dependencies = parse vx input, workers = n }
    where
      vx s = (s, duration s)
      duration = (+) (1 + extra) . fromJust . flip elemIndex alphabet
      alphabet = ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z"]

  isDone :: BState -> Bool
  isDone s = noDeps && noInProgress
    where
      noDeps = (==) 0 . order . dependencies $ s
      noInProgress = not . any ((/=) 0 . snd) . inProgress $ s

  next :: BState -> BState
  next s
    | canAssignWork s   = assignWork s
    | canCompleteWork s = completeWork s
    | canAdvance s      = advance s
    | now s > 1000 = error ("an infinite loop seems to have been entered with current state: " ++ show s)
    | otherwise = error ("no available (implemented) actions for the current state: " ++ show s)

  available :: BState -> Int
  available (BState { workers = n, inProgress = ip, dependencies = _, now = _ }) = workersAvailable
    where
      workersAvailable = (-) n . length . filter busy $ ip
        where
          busy = positive . snd
          positive x = x > 0

  canAdvance :: BState -> Bool
  canAdvance s = any ((<) 0 . snd) . inProgress $ s

  advance :: BState -> BState
  advance s = now' ((+) t) $ inProgress' (fmap advanceWork) s
    where
      t = minimum . fmap snd . inProgress $ s
      advanceWork (s', t') = (s', t' - t)

  canAssignWork :: BState -> Bool
  canAssignWork s = hasWorkers && hasPendingWork
    where
      hasWorkers = available s > 0
      assignments = ready . dependencies $ s
      ongoing = fmap fst . inProgress $ s
      hasPendingWork = any (not . flip elem ongoing . fst) assignments

  assignWork :: BState -> BState
  assignWork s = inProgress' ((++) todo) s
    where
      ongoing = fmap fst . inProgress $ s
      pendingWork = filter (not . flip elem ongoing . fst) . ready . dependencies $ s
      todo = take (available s) pendingWork

  canCompleteWork :: BState -> Bool
  canCompleteWork = any ((==) 0 . snd) . inProgress

  completeWork :: BState -> BState
  completeWork s = inProgress' removeFromInProgress $ dependencies' removeFromDependencies s
    where
      completed = fmap fst . filter ((==) 0 . snd) . inProgress $ s
      removeFromInProgress = filter ((/=) 0 . snd)
      toRemove = filter (flip elem completed . fst) . vertices . dependencies $ s
      removeFromDependencies = removeVertices toRemove

  ready :: (Ord v, Hashable v) => DependencyTree v -> [v]
  ready g = sort . filter (isSink g) $ vertices g

  type DependencyTree a = DGraph a ()
  parse :: (Hashable a, Eq a) => (String -> a) -> [String] -> DependencyTree a
  parse f = fromArcsList . fmap parseStep
    where
      parseStep step = arc
        where
          parsed = Parse.expect "Step " step
            >>= Parse.char
            >>= Parse.expect " must be finished before step "
            >>= Parse.char
            >>= Parse.expect " can begin."
          construct [dependency, dependent] = f dependent --> f dependency
          construct x = error ("expected [from,to] but got " ++ show x)
          arc = Parse.extract construct parsed
