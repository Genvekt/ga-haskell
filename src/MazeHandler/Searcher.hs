module MazeHandler.Searcher where

import Data.List
import DataStructures.Maze

-- | Check if Tile is in maze
contains
 :: Maze      -- ^ Where to search
 -> Tile      -- ^ What to search
 -> Bool
contains maze tile
 | result == 1 = True
 | otherwise = False
    where
     result = sum(map found_exit ( map (find isExit) maze))
     found_exit Nothing = 0
     found_exit _ = 1
     isExit Exit = True
     isExit _ = False


-- | Get tile by coordinates
whatCell
 :: [[a]]       -- ^ Matrix of cells
 -> Coords     -- ^ Where to look
 -> Maybe a
whatCell maze (x,y) = getColumn row
  where
   row = lookup x (zip [1..] maze)
   getColumn Nothing = Nothing
   getColumn (Just r) = lookup y (zip [1..] r)


 -- | Get coords of the first desired tile
coordsOfTile :: Maze -> Tile ->Maybe Coords
coordsOfTile maze tile = extract (find (equals Exit) numerated_cells)
  where
   numerated_cells
    = concat (map (\(i,row) -> zip [i,i..] row) (zip [1..] (map (zip [1..]) maze)))
   extract Nothing = Nothing
   extract (Just (i,(j,_))) = Just (i,j)
   equals Exit (_,(_,Exit)) = True
   equals Wall (_,(_,Wall)) = True
   equals FakeExit (_,(_,FakeExit)) = True
   equals Floor (_,(_,Floor)) = True
   equals _ _ = False

-- | Get height and width of maze
mazeShape :: Maze -> (Int,Int)
mazeShape maze = (height, width)
    where
     height = length maze
     width = maximum (map length maze)
