module Apps.GAMazeSolver.Interaction where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss

import MazeHandler.Generator
import System.Random

import DataStructures.Maze
import DataStructures.Agent
import Visualiser.System


import MazeHandler.Transformer
import Apps.GAMazeSolver.Settings

import Agent.MazeSolver


visualization ::(Int,Int) -> Int -> [Gene] ->  IO()
visualization size seed genes = play window white 10 (initHeros genes) (drawSystem currentMaze) (handleAction currentMaze) updateHeros
 where
   (currentMaze,_) = generateMaze size (mkStdGen seed)

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)
-----------------------------     Interaction    -------------------------------

initializeHero :: Gene -> Hero
initializeHero gene = Hero gene [] startCoords 0

initHeros :: [Gene] -> [Hero]
initHeros genes = map initializeHero genes

updateHeros :: Float -> [Hero] -> [Hero]
updateHeros _ heros = heros

handleAction :: Maze -> (Event -> [Hero] -> [Hero])
handleAction maze (EventKey (Char 'q') Down _ _) = map (makeMove maze)
handleAction maze _ = id

                              -- ^ The maze where all out of hero's vision is wall
