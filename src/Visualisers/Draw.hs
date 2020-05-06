module Visualisers.Draw where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Data.Monoid ((<>))

import DataStructures.Maze
import DataStructures.GA
import Maze
-----------------------------  Draw Functions ----------------------------------

drawSystem:: Maze -> [Hero] -> Picture
drawSystem maze heros =
 scale size size (translate (-i/2) (-j/2) ( drawBoard maze  <> mconcat (map drawHero heros) ))
  where
   (height,width) = mazeShape maze
   i = fromIntegral height
   j = fromIntegral width
   size
    | i > j = j/i *50
    | otherwise = i/j *50

drawHero :: Hero -> Picture
drawHero (Hero (vision, _) history (x,y) _) =
 (color green (drawCellAt y x Exit)) <>
 drawMemory history <>
 drawVision (x,y) vision


drawMemory :: [Coords] -> Picture
drawMemory memory = pictures (map showCell agedMoves)
 where
  agedMoves = zip [1..] memory
  showCell (age, (x,y)) =(color ( withAlpha (1 -age/ fullMemory) yellow) (drawCellAt y x Wall))
  fullMemory = (fromIntegral (length(memory)))


drawVision :: Coords -> Int -> Picture
drawVision (x,y) radius = translate j i (color (withAlpha 0.3 (blue)) (rectangleSolid r r))
 where
  i = fromIntegral x
  j = fromIntegral y
  r = fromIntegral radius


-- | Draw a rectangular board.
drawBoard :: Maze -> Picture
drawBoard board = pictures (map map_drawing (concat indixed_board))
 where
  indixed_board = addCoords board
  map_drawing (cell,(x,y)) = drawCellAt x y cell


-- | Draw a single mark (X or O).
drawMark :: Tile -> Picture
drawMark Wall = rectangleSolid 1 1
drawMark Floor = color white (rectangleSolid 1 1)
drawMark Exit = color yellow (thickCircle 0.2 0.3)
drawMark FakeExit = color orange (thickCircle 0.2 0.3)


-- | Draw one board cell at given coordinates.
drawCellAt :: Int -> Int -> Tile -> Picture
drawCellAt i j cell = translate x y
 (rectangleSolid 1 1 <> cellPicture)
  where

   x = fromIntegral i
   y = fromIntegral j
   cellPicture = drawMark cell
