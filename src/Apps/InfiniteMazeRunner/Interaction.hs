module Apps.InfiniteMazeRunner.Interaction where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import MazeHandler.Generator
import MazeHandler.Searcher
import Agent.MazeSolver
import Visualiser.System
import System.Random
import DataStructures.Maze
import DataStructures.Agent
import Data.Monoid ((<>))

data State = State Maze Maze Hero Int StdGen Int

infiniteMaze :: (Int, Int)-> IO()
infiniteMaze size = play window white 10 (initialState size) drawState handleAction updateState

window :: Display
window = InWindow "Nice Window" (600, 600) (10, 10)

initialState :: (Int, Int)-> State
initialState size = State maze1 maze2 hero 0 generator2 0
  where
    (maze1, generator1) = generateMaze size (mkStdGen 42)
    (maze2, generator2) = generateMaze size generator1
    hero = Hero (5,10) [] (2,2) 0

updateState :: Float -> State -> State
updateState _ state = state

drawState :: State -> Picture
drawState (State m1 m2 hero shift _ _) = drawSystem maze [hero]
 where
   maze = makeMaze m1 m2 shift

handleAction :: Event -> State -> State
handleAction (EventKey (Char 'a') Down _ _) state =
   State maze1 maze2 updHero shift gen timer
     where
       (State maze1 maze2 hero shift gen timer) = shiftSystem state
       maze = makeMaze maze1 maze2 shift
       updHero = updateHero hero maze
handleAction _ state = state

shiftSystem:: State -> State
shiftSystem (State m1 m2 hero shift gen 15) = State maze1 maze2 shiftedHero newshift newGen 0
  where
    (maze1, maze2, newGen, newshift) = updateMaze (m1, m2, gen, shift)
    shiftedHero = shiftHero hero
shiftSystem (State m1 m2 hero shift gen timer) = State m1 m2 hero shift gen (timer+1)

updateMaze :: (Maze, Maze, StdGen, Int) -> (Maze, Maze, StdGen, Int)
updateMaze (m1, m2, gen, shift)
 | shift < width-2 = (m1, m2, gen, (shift+1))
 | otherwise = (m2, newMaze, updGen, 1)
  where
    (height, width) = mazeShape m1
    (newMaze, updGen) = generateMaze (height, width) gen

shiftHero:: Hero -> Hero
shiftHero (Hero gene path (x,y) lives) = (Hero gene shiftedPath (x,y-1) lives)
  where
    shiftedPath = map (\(x,y) -> (x,y-1)) path

updateHero::Hero->Maze->Hero
updateHero (Hero gene path (x,y) lives) maze
 | y <0 = Hero gene path (x,y) lives
 | otherwise = Hero gene path' (x',y') (lives+1)
  where
   (Hero _ path' (x',y') _) = makeMove maze (Hero gene path (x,y) lives)

makeMaze :: Maze -> Maze -> Int -> Maze
makeMaze maze1 maze2 shift= union
 where
   (_, width) = mazeShape maze1
   part1 = map ((drop (shift)).(take (width-1))) maze1
   part2 = map ((take (shift).(drop 1))) maze2
   union = map (\(row1, row2) -> [Wall] <> row1 <> row2 <> [Wall]) (zip part1 part2)
