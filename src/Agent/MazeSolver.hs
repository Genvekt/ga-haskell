module Agent.MazeSolver where

import Data.Maybe
import Data.List

import DataStructures.Maze
import DataStructures.Agent

import MazeHandler.Searcher
import MazeHandler.Transformer
import MazeHandler.DFS

-- | Define where hero will stop
runSimulation
 :: Hero    -- ^ State of the gene
 -> Maze    -- ^ Maze to solve
 -> Coords  -- ^ The end point where hero dies
runSimulation (Hero _ _ position 0) _ = position                                 -- ^ End simulation then hero is dead
runSimulation (Hero (vision, memory) history position health) maze =
 runSimulation (Hero (vision, memory) new_history next_position (health-1)) maze -- ^ Continue simulation from next position
   where
    next_position =
      nextMove (Hero (vision, memory) history position health) visible_maze      -- ^ The position hero chooses to go to now
    new_history = reverse( take memory ( position : reverse history))             -- ^ The updated hero's memory about his moves
    visible_maze = cutMaze maze position vision                                  -- ^ The maze where all out of hero's vision is wall

makeMove ::Maze-> Hero -> Hero
makeMove maze (Hero (vision, memory) history position health) = (Hero (vision, memory) new_history next_position health)
 where
  next_position =
      nextMove (Hero (vision, memory) history position health) visible_maze      -- ^ The position hero chooses to go to now
  new_history = reverse( take memory ( position : reverse history))             -- ^ The updated hero's memory about his moves
  visible_maze = cutMaze maze position vision
  
-- | Determine where hero will go next at this particular moment of time
nextMove
 :: Hero   -- ^ State of the gene
 -> Maze   -- ^ Maze to solve
 -> Coords -- ^ The next position that hero choose
nextMove (Hero (vision, memory) history (x,y) health) maze
 | stopOn Exit (whatCell maze (x,y)) = (x,y)                                    -- ^ If current position is exit, stay on this position
 | contains maze Exit = go_to (listToMaybe coords_to_exit)                        -- ^ If exit is in vision area, try go first leading to it direction
 | otherwise =
     decideMove (Hero (vision, memory) history (x,y) health) modified_maze       -- ^ Choose good enough move
  where
   coords_to_exit = runDfs maze (stopOn Exit) (x,y)                             -- ^ Neighbour coords which lead to exit
   go_to (Just move) = move                                                       -- ^ First element of coords to exit
   go_to Nothing =                                                                -- ^ If there is no coords going yo exit
        decideMove (Hero (vision, memory) history (x,y) health) modified_maze    -- ^  choose good enough move
   modified_maze = map (mapExitsRow (x,y) vision) (zip [1..] maze)              -- ^ Maze where all Floors that lie on edge of hero vision area
                                                                                  -- ^  are market as FakeExits

-- | Determine good enough move at this particular moment of time
decideMove
 :: Hero     -- ^ State of the gene
 -> Maze     -- ^ Maze to solve
 -> Coords   -- ^ The good enough move
decideMove (Hero _ history (x,y) _) maze =
 extractMove (listToMaybe sorted_coords)
 where
  good_coords = runDfs maze (stopOn FakeExit) (x,y)                             -- ^ Coords that leads to any FakeExit
  ages = map (indexOf (zip [1..] history)) good_coords                           -- ^ Determine how old the coords are in terms of your memory
  sorted_coords = sortOn fst (zip ages good_coords)                               -- ^ Sort coords in unknow -> oldest -> newest visited order
  extractMove (Just (_,move)) = move                                                -- ^ Good move == move that is far away in memory
  extractMove Nothing = (x,y)                                                       -- ^ If there is no choice, stay on current position


-- | Get index of element, 0 if it is not present
indexOf
 :: [(Int, Coords)]   -- ^ Indexed array
 -> Coords            -- ^ What to search
 -> Int               -- ^ Index
indexOf [] _ = 0
indexOf ((index,(x,y)):xs) (x0,y0)
 | x0 == x && y0 == y = index
 | otherwise = indexOf xs (x0,y0)


-- | DFS Stop Conditions
stopOn
 :: Tile->   -- ^ Desired Tile
 Maybe Tile  -- ^ Current Tile
 -> Bool
stopOn Exit (Just Exit) = True
stopOn Wall (Just Wall) = True
stopOn Floor (Just Floor) = True
stopOn FakeExit (Just FakeExit) = True
stopOn _ _ = False
