module Output where

import           Data.Function       (on)
import           Data.List           (groupBy, intercalate, sortBy)
import           Data.Map            (toList)
import           Data.Tuple          (swap)
import           Solver
import           System.Console.ANSI
import           Text.Printf

setColor :: ColorIntensity -> Color -> IO ()
setColor i c = setSGR [SetColor Foreground i c]

goTo :: Int -> Int -> IO ()
goTo x y = setCursorPosition (y + 2) x

drawMap :: Dungeon -> IO ()
drawMap dungeon = do
  sequence_ . fmap drawTerrain . toList $ dungeon
  where
    drawTerrain :: (Coord, Terrain) -> IO ()
    -- drawTerrain ((x,y), Wall)   = do
    --   setColor Dull White
    --   goTo x y
    --   putChar '#'
    drawTerrain (_, Wall) = do
      return ()
    drawTerrain ((x, y), Cavern) = do
      setColor Dull White
      goTo x y
      putChar '.'

drawUnits :: [Unit] -> IO ()
drawUnits units = do
  sequence_ . fmap drawOne $ units
  where
    drawOne :: Unit -> IO ()
    drawOne u = drawUnit (defaultColor u) u

defaultColor :: Unit -> Color
defaultColor (Unit Elf _ _ _)    = Red
defaultColor (Unit Goblin _ _ _) = Green

drawUnit :: Color -> Unit -> IO ()
drawUnit clr (Unit r (x, y) _ _) = do
  setColor Vivid clr
  goTo x y
  putChar $ defaultChar r
  where
    defaultChar Elf    = 'E'
    defaultChar Goblin = 'G'

markUnit :: Unit -> IO ()
markUnit u = do
  drawUnit Yellow u

showState :: Int -> Dungeon -> [Unit] -> IO ()
showState r dungeon units = do
  let (x, y) = size dungeon
  clearScreen
  putStrLn ""
  showRoundNumber r
  drawMap dungeon
  drawUnits units
  drawHitPoints (x + 3) units
  goTo 0 (y + 1)
  putStrLn ""

showRoundNumber :: Int -> IO ()
showRoundNumber 0 = do
  setColor Vivid White
  putStrLn "Initially:"
showRoundNumber r = do
  setColor Vivid White
  putStrLn $ printf "After %d rounds:" r

drawHitPoints :: Int -> [Unit] -> IO ()
drawHitPoints offset units = do
  let sorted =
        groupBy ((==) `on` (snd . position)) .
        sortBy (compare `on` (swap . position)) $
        units
  setColor Dull White
  sequence_ . fmap drawLineHp $ sorted
  where
    drawLineHp row = do
      let y = snd . position . head $ row
      goTo (offset + 1) y
      putStr . intercalate ", " . fmap showUnitHp $ row
    showUnitHp (Unit Elf _ _ hp)    = printf "E(%d)" hp
    showUnitHp (Unit Goblin _ _ hp) = printf "G(%d)" hp

endCombat :: Dungeon -> IO ()
endCombat dungeon = do
  let (_, y) = size dungeon
  setColor Vivid White
  goTo 0 (y + 1)
  putStrLn ""
