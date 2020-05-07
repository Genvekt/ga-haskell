module Visualiser.Maze where

import Graphics.Gloss
import Data.Monoid ((<>))

import DataStructures.Maze
import MazeHandler.Transformer

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
