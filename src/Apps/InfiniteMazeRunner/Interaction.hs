module Apps.InfiniteMazeRunner.Interaction where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss

import Agent.InfMazeSolver

import MazeHandler.Generator

import Visualiser.System

import DataStructures.Maze
import DataStructures.Agent

agentsInMaze :: (Int, Int) -> [Gene] -> IO()
agentsInMaze size agents = play window white 10 (initStates size agents) drawState handleAction updateState


infiniteMaze :: (Int, Int)-> IO()
infiniteMaze size = play window white 10 (initialState size) drawState handleAction updateState

window :: Display
window = InWindow "Nice Window" (600, 600) (10, 10)

initialState :: (Int, Int)-> [State]
initialState size = [State infMaze hero1,State infMaze hero2]
  where
    infMaze = generateInfMaze size
    hero1 = Hero (5,10) [] (2,2) 0
    hero2 = Hero (10,5) [] (2,2) 0

initStates :: (Int, Int) -> [Gene] -> [State]
initStates size genes = map (\gene -> (State maze (Hero gene [] (2,2) 0))) genes
 where
   maze = generateInfMaze size

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
