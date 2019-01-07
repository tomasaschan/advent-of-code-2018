module Output where

import Solvers.Dec15

import System.Console.ANSI

drawMap :: Dungeon -> IO ()
drawMap dungeon = do
  sequence_ . fmap drawTerrain . toList $ dungeon
  where
    drawTerrain :: (Coord, Terrain) -> IO ()
    drawTerrain (_    , Wall)   = return ()
    drawTerrain ((x,y), Cavern) = do
      setSGR [SetColor Foreground Dull White]
      setCursorPosition x y
      putChar '.'

drawUnits :: [Unit] -> IO ()
drawUnits units = do
  sequence_ . fmap drawUnit $ units
  where
    drawUnit :: Unit -> IO ()
    drawUnit (Unit Elf (x,y)) = do
      setSGR [SetColor Foreground Vivid Red]
      setCursorPosition x y
      putChar 'E'
    drawUnit (Unit Goblin (x,y)) = do
      setSGR [SetColor Foreground Vivid Green]
      setCursorPosition x y
      putChar 'G'

showState :: World -> IO ()
showState (World dungeon units) = do
  clearScreen
  drawMap dungeon
  drawUnits units
  let (_,y) = size dungeon
  setCursorPosition 0 (y+1)
  putStrLn ""


endProgram :: World -> IO ()
endProgram (World dungeon _) = do
  let (_,y) = size dungeon
  setCursorPosition 0 0
  putStrLn $ replicate (y+1) '\n'
