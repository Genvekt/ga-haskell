module MazeHandler.DFS where

import Data.List

import DataStructures.Maze
import MazeHandler.Searcher

-- | Get list of coordinates that leads to desired Tile
runDfs
 :: Maze                 -- ^ Where to search
 -> (Maybe Tile -> Bool) -- ^ Stop condition
 -> Coords               -- ^ Start position
 -> [Coords]             -- ^ Coordinates that leads to desired Tile
runDfs maze stop_condition (x,y) =
   filter (hasPath maze stop_condition (x,y)) coords                            -- ^ Filter only coords that lead to desired Tile
 where
  coords = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]                                     -- ^ Try all 4 directions from start position

-- | Check if move leads to desired Tile
hasPath
 :: Maze                  -- ^ Where to search
 -> (Maybe Tile -> Bool)  -- ^ Stop condition
 -> Coords                -- ^ Start position
 -> (Coords -> Bool)
hasPath maze stop_condition from to = dfs maze to [from] stop_condition



 -- | Check if path leads to desired Tile
dfs
  :: Maze                 -- ^ Where to search
  -> Coords               -- ^ Current position
  -> [Coords]             -- ^ Tracked path
  -> (Maybe Tile -> Bool) -- ^ Stop condition
  -> Bool
dfs maze (x,y) from isEnd
  | isWall (whatCell maze (x,y)) = False                                        -- ^ If current position is wall - there is not way
  | isEnd (whatCell maze (x,y)) = True                                          -- ^ If we found desired Tile - way found
  | otherwise = canGoTo (x-1,y) || canGoTo (x+1,y) ||                         -- ^ Check all 4 directions
                canGoTo (x,y-1) || canGoTo (x,y+1)
    where
     isWall Nothing = True
     isWall (Just Wall) = True
     isWall (Just _) = False
     canGoTo (x_next, y_next)
      | inMemory (find (\(x0,y0) -> (x0 == x_next && y0==y_next)) from) = False    -- ^ If next position already wisited - there is no way
      | otherwise = dfs maze (x_next,y_next) ((x,y):from) isEnd                    -- ^ Go deeper
     inMemory Nothing = False
     inMemory _ = True
