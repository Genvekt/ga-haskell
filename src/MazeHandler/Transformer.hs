module MazeHandler.Transformer where

import Data.List
import DataStructures.Maze

---------------------  Maze functions/Transformations --------------------------

-- | Change all Floors that lie on adge of hero vision to FakeExit
mapExitsRow
 :: Coords          -- ^ Hero position
 -> Int             -- ^ Hero vision
 -> (Int, [Tile])   -- ^ Numirated row
 -> [Tile]          -- ^ Transformed row
mapExitsRow (x,y) r (index, row)
 | index == (x+r) || index == (x-r) = map to_fake (zip [1..] row)                -- ^ Change all Floors to FakeExit in (x-vision) and (x+vision) rows
 | (index > (x+r)) || (index < (x-r)) = row                                      -- ^ Do not change rows outside of the hero's vision region
 | otherwise = map map_exits_cell (zip [1..] row)                                -- ^ Change boundary cells in inner rows
 where
  to_fake (_,Floor) = FakeExit
  to_fake (_,t) = t
  map_exits_cell (i,Floor)
   | i == (y+r) || i == (y-r) = FakeExit                                         -- ^ Change boundary cell if it is Floor
   | otherwise = Floor                                                           -- ^ Do not change other cells
  map_exits_cell (_,t) = t


-- | Change everyting out of visible region to walls in matrix
cutMaze
 :: Maze   -- ^ Matrix to transform
 -> Coords -- ^ Hero's position
 -> Int    -- ^ Hero's vision
 -> Maze   -- ^ Transformed maze
cutMaze maze (x,y) radius = map (mapEmptyRow (x,y) radius)  numerated_maze
 where
  numerated_maze = zip [1..] (map (zip [1..]) maze)

-- | Change everyting out of visible region to walls in row
mapEmptyRow :: Coords -> Int -> (Int, [(Int,Tile)]) -> [Tile]
mapEmptyRow (x, y) r (index, row)
 | (index < (x-r)) || (index>(x+r)) =
      take (length(row)) (iterate buildWall Wall)                               -- ^ If row is out of visible region, substitute it
 | otherwise = map (mapEmptyCol y r) row
  where
   buildWall Wall = Wall
   mapEmptyCol y r (index, cell)                                               -- ^ Map invisible sells to Wall
    | (index < (y-r)) || (index>(y+r)) = Wall
    | otherwise = cell

-- | Add coordinated to all cells of the board
addCoords :: Maze -> [[(Tile, (Int,Int))]]
addCoords board = map addCoordsRow (zip board [1..])


-- | Add coordinated to all cells in the row
addCoordsRow :: ([Tile], Int) -> [(Tile, (Int,Int))]
addCoordsRow (row, index) = zip row (zip [1..] (to_list index))
 where
  to_list a = a:to_list a

changeCellAt
 :: [[a]]
 -> Coords
 -> a
 -> [[a]]

-- Change cell in matrix to some value
changeCellAt matrix (i,j) value = map changeRow(zip [1..] matrix)
 where
   changeRow (rowID, row)
    | rowID == i = map changeCell (zip [1..] row)
    | otherwise = row
   changeCell (cellID, cell)
    | cellID == j = value
    | otherwise = cell
