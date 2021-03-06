module Apps.InfiniteMazeRunner.Interaction where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss

import Agent.InfMazeSolver

import MazeHandler.Generator

import Visualiser.System

import DataStructures.Maze
import DataStructures.Agent

-- | Run several agents in one maze
agentsInMaze :: (Int, Int) -> Int -> [Gene] -> IO()
agentsInMaze size seed agents = play window white 10 (initStates size seed agents) drawState handleAction updateState

-- | Run determined agents in one maze
infiniteMaze :: (Int, Int) -> Int -> IO()
infiniteMaze size seed = play window white 10 (initialState size seed) drawState handleAction updateState

window :: Display
window = InWindow "Nice Window" (1000, 1500) (10, 10)

-- | The initial state of the system
initialState :: (Int, Int)-> Int-> [State]
initialState size seed= [State infMaze hero1,State infMaze hero2]
  where
    infMaze = generateInfMaze size seed
    hero1 = Hero (5,10) [] (2,2) 0
    hero2 = Hero (10,5) [] (2,2) 0

-- | Initial state with given agents
initStates :: (Int, Int) -> Int-> [Gene] -> [State]
initStates size seed genes = map (\gene -> (State maze (Hero gene [] (2,2) 0))) genes
 where
   maze = generateInfMaze size seed

updateState :: Float -> [State] -> [State]
updateState _ state = state

drawState :: [State] -> Picture
drawState states = drawSystem maze heros
 where
   heros = map (\(State _ hero) -> hero) states
   [(State (InfMaze m1 m2 shift _ _) _)] = take 1 states
   maze = makeMaze m1 m2 shift

handleAction :: Event -> [State] -> [State]
handleAction (EventKey (Char 'a') Down _ _) state = map moveInInfMaze state
handleAction _ state = state
