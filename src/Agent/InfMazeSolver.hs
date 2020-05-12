module Agent.InfMazeSolver where

import DataStructures.Maze
import DataStructures.Agent

import MazeHandler.Generator
import MazeHandler.Searcher

import Agent.MazeSolver

-- | Run in infinite maze untile the stop criteria is met
runInInfMaze
  :: Hero           -- ^ State of the gene
  -> InfMaze        -- ^ Maze to solve
  -> (Hero -> Bool) -- ^ Stop criteria
  -> Hero           -- ^ The end point where hero dies
runInInfMaze (Hero (vis, mem) hist pos health) maze isStop
 | isStop (Hero (vis, mem) hist pos health) =                                   -- ^ End simulation on stopp criteria
   (Hero (vis, mem) hist pos health)
 | otherwise = runInInfMaze updHero updMaze isStop                              -- ^ Continue simulation from next position
     where
      (State updMaze updHero) =
        moveInInfMaze (State  maze (Hero (vis, mem) hist pos health) )

-- | Make move in the infinite maze
moveInInfMaze :: State -> State
moveInInfMaze state = State (InfMaze maze1 maze2 shift gen timer) updHero
  where
    (State (InfMaze maze1 maze2 shift gen timer) hero) = shiftSystem state
    maze = makeMaze maze1 maze2 shift
    updHero = updateHero hero maze

-- | Shift maze amd hero elements if timer is said to do so
shiftSystem:: State -> State
shiftSystem (State (InfMaze m1 m2 shift gen 15) hero) =
  State (InfMaze maze1 maze2 newshift newGen 0) shiftedHero
    where
      (InfMaze maze1 maze2 newshift newGen _) =
        updateMaze (InfMaze m1 m2 shift gen 15)
      shiftedHero = shiftHero hero

shiftSystem (State (InfMaze m1 m2 shift gen timer) hero) =
  State (InfMaze m1 m2 shift gen (timer+1)) hero

-- | Create next part of infinite maze
updateMaze :: InfMaze -> InfMaze
updateMaze (InfMaze m1 m2 shift gen t)
 | shift < width-2 = (InfMaze m1 m2 (shift+1) gen t)
 | otherwise = (InfMaze m2 newMaze 1 updGen t)
  where
    (height, width) = mazeShape m1
    (newMaze, updGen) = generateMaze (height, width) gen

-- | Shift all coordinates in hero parts to the left
shiftHero:: Hero -> Hero
shiftHero (Hero gene path (x,y) lives) = (Hero gene shiftedPath (x,y-1) lives)
  where
    shiftedPath = map (\(x,y) -> (x,y-1)) path

-- | Make move in visible part of maze
updateHero::Hero->Maze->Hero
updateHero (Hero gene path (x,y) lives) maze
 | y <0 = Hero gene path (x,y) lives
 | otherwise = Hero gene path' (x',y') (lives+1)
  where
   (Hero _ path' (x',y') _) = makeMove maze (Hero gene path (x,y) lives)
