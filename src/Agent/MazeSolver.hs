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
 -> (Hero -> Bool) -- ^ Stop criteria
 -> Hero  -- ^ The end point where hero dies
runSimulation (Hero (vis, mem) hist pos health) maze isStop
 | isStop (Hero (vis, mem) hist pos health) =                                   -- ^ End simulation on stopp criteria
   (Hero (vis, mem) hist pos health)
 | otherwise =
    runSimulation (Hero (vis, mem) new_hist next_pos (health-1)) maze  isStop   -- ^ Continue simulation from next position
     where
      next_pos =
        nextMove (Hero (vis, mem) hist pos health) visible_maze      -- ^ The position hero chooses to go to now
      new_hist = reverse( take mem ( pos : reverse hist))             -- ^ The updated hero's memory about his moves
      visible_maze = cutMaze maze pos vis                                  -- ^ The maze where all out of hero's vision is wall

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
 -- | contains maze Exit = go_to (listToMaybe coords_to_exit)                        -- ^ If exit is in vision area, try go first leading to it direction
 | otherwise =
     decideMove (Hero (vision, memory) history (x,y) health) modified_maze       -- ^ Choose good enough move
  where
   modified_maze = map (mapExitsRow (x,y) vision) (zip [1..] maze)              -- ^ Maze where all Floors that lie on edge of hero vision area
                                                                                  -- ^  are market as FakeExits

-- | Determine good enough move at this particular moment of time
decideMove
 :: Hero     -- ^ State of the gene
 -> Maze     -- ^ Maze to solve
 -> Coords   -- ^ The good enough move
decideMove (Hero (vision, memory) history (x,y) _) maze =
 extractMove (listToMaybe sorted_coords)
 where
  good_coords = runPathFinder maze vision memory (stopOn FakeExit) (x,y)                             -- ^ Coords that leads to any FakeExit
  ages = map (indexOf (zip [1..] history)) good_coords                           -- ^ Determine how old the coords are in terms of your memory
  sorted_coords = sortOn fst (zip ages good_coords)                               -- ^ Sort coords in unknow -> oldest -> newest visited order
  extractMove (Just (_,move)) = move                                                -- ^ Good move == move that is far away in memory
  extractMove Nothing = (x,y)                                                       -- ^ If there is no choice, stay on current position

  -- | Get list of coordinates that leads to desired Tile
  runPathFinder
   :: Maze                 -- ^ Where to search
   -> Int
   -> Int
   -> (Maybe Tile -> Bool) -- ^ Stop condition
   -> Coords               -- ^ Start position
   -> [Coords]             -- ^ Coordinates that leads to desired Tile
  runPathFinder maze vison memory stop_condition (x,y) =
     filter (hasPath maze stop_condition (x,y)) coords                            -- ^ Filter only coords that lead to desired Tile
   where
    coords
      | (vison `mod` 2 == 0) && (memory `mod` 2 == 0) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]                                     -- ^ Try all 4 directions from start position
      | (vison `mod` 2 == 1) && (memory `mod` 2 == 0) = [(x-1,y),(x,y+1),(x,y-1),(x+1,y)]
      | (vison `mod` 2 == 0) && (memory `mod` 2 == 1) = [(x,y+1),(x,y-1),(x+1,y),(x-1,y)]
      | (vison `mod` 2 == 1) && (memory `mod` 2 == 1) = [(x,y-1),(x+1,y),(x-1,y),(x,y+1)]
      | otherwise = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

  -- | Check if move leads to desired Tile
  hasPath
   :: Maze                  -- ^ Where to search
   -> (Maybe Tile -> Bool)  -- ^ Stop condition
   -> Coords                -- ^ Start position
   -> (Coords -> Bool)
  hasPath maze stop_condition (x,y) (x_next,y_next) =
   findPath maze (x_next,y_next) stop_condition (x_next + x_next - x ,y_next + y_next - y)

  -- | Function to determine the available path
  findPath
    :: Maze
    -> Coords
    -> (Maybe Tile -> Bool)
    -> Coords
    -> Bool
  findPath maze (x,y) isEnd (x_next,y_next)
   | isWall (whatCell maze (x,y)) = False
   | isWay (whatCell maze (x,y + (x_next - x))) && (x_next /= x) = True
   | isWay (whatCell maze (x,y - (x_next - x))) && (x_next /= x) = True
   | isWay (whatCell maze (x + (y_next - y),y)) && (y_next /= y) = True
   | isWay (whatCell maze (x - (y_next - y),y)) && (y_next /= y) = True
   | isEnd (whatCell maze (x,y)) = True
   | isExit (whatCell maze (x,y)) = True
   | otherwise = findPath maze (x_next,y_next) isEnd (x_next + x_next - x ,y_next + y_next - y)
   where
    isWall Nothing = True
    isWall (Just Wall) = True
    isWall (Just _) = False
    isWay Nothing = False
    isWay (Just Floor) = True
    isWay (Just FakeExit) = True
    isWay (Just _) = False
    isExit Nothing = False
    isExit (Just Exit) = True
    isExit (Just _) = False


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
