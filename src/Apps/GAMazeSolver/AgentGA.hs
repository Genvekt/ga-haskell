module Apps.GAMazeSolver.AgentGA where

import Apps.GAMazeSolver.Settings
import Data.List
import Data.Maybe
import System.Random

import Maze
import DataStructures.Maze
import DataStructures.GA


------------------- Initial Population Builder ---------------------------------

-- | The starting population
initPopulation
 :: Int         -- ^ The number of genetical points
 -> Int         -- ^ The size of the population
 -> [Gene]      -- ^ Generated opulation
initPopulation amount size = take size (map makeGene [0,1..])
 where
  makeGene x = (x, amount-x)

------------------------ Fit Function ------------------------------------------

-- | Write hero walk through maze and return how far from finish he dies
evaluateHero :: Maze ->(Gene -> Double)
evaluateHero maze gene = distance maze ( runSimulation (Hero gene [] startCoords heroHealth) maze)


-- | Distance from given position to the exit of maze
distance
 :: Maze
 -> Coords  -- ^ Position from which the distance is meatured
 -> Double  -- ^ Distance
distance maze (x, y) = sqrt (fromIntegral ((x0 - x)^2 + (y0 - y)^2))
 where
  stop (Just coords) = coords
  stop Nothing = (10000,10000)
  exit = coordsOfTile maze Exit
  (x0, y0) = stop exit


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


-- | Determine where hero will go next at this particular moment of time
nextMove
 :: Hero   -- ^ State of the gene
 -> Maze   -- ^ Maze to solve
 -> Coords -- ^ The next position that hero choose
nextMove (Hero (vision, memory) history (x,y) health) maze
 | stopOn Exit (whatTile maze (x,y)) = (x,y)                                    -- ^ If current position is exit, stay on this position
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
 | isWall (whatTile maze (x,y)) = False                                        -- ^ If current position is wall - there is not way
 | isEnd (whatTile maze (x,y)) = True                                          -- ^ If we found desired Tile - way found
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


------------------------ Cross Over Function -----------------------------------

-- | Generate new genes from given ones                                           -- ^ Takes n genes
crossGenes :: [Gene] -> [Gene]                                                   -- ^ Returns n new crossed by mean genes
crossGenes [] = []
crossGenes [(gf1, gs1)] = [( mean gf1 gs1, mean gf1 (mean gf1 gs1))]
crossGenes [(gf1, gs1), (gf2, gs2)] = [(mean gf1 gf2, mean gs1 gs2)]
                                    ++ crossGenes([(gf2, gs2)])

crossGenes (gene1:gene2:genes)      = [(mean f1 f2, mean s1 s2)]
                                    ++ [(mean f1 f3, mean s1 s3)]
                                    ++ crossGenes([gene2] ++ genes)
  where
    f1 = fst gene1
    f2 = fst gene2
    f3 = fst (head genes)
    s1 = snd gene1
    s2 = snd gene2
    s3 = snd (head genes)

-- | Mean value of two integers x and y
mean :: Int -> Int -> Int
mean x y = round( fromIntegral(x+y) / 2)

------------------------- Mutation Function ------------------------------------

-- | Mutate each element of gene with given probability                           -- ^
mutateGene
 :: Double         -- ^ The treshold for mutation to happen
 -> (Double,Gene)  -- ^ Gene to mutate with its probability to mutate
 -> Gene           -- ^ Mutated gene
mutateGene  treshold (prob,(vision,memory))
  | treshold > prob = (vision,memory)                                            -- ^ No mutation happens
  | prob < sub_treshold = update_gene_on growth
  | otherwise = update_gene_on (-growth)
    where
      total_score = genePoints
      growth = 1
      sub_treshold = (1 - treshold)/2 + treshold
      update_gene_on value
       | vision + value > total_score = (total_score, 0)
       | vision + value < 0 = (0, total_score)
       | otherwise = (vision + value, total_score - (vision + value))
