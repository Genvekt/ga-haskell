module Main where
-- import Apps.GAMazeSolver.Run
import Apps.InfiniteMazeRunner.Run
--import Apps.GAMazeSolver.Run
import Agent.InfMazeSolver
import Apps.InfiniteMazeRunner.GA
import MazeHandler.Generator
import DataStructures.Agent




main :: IO ()
--main = solution3
main = runAlgorithm (11,51)
--main = putStrLn ( show (runInInfMaze (Hero (0,0) [] (2,2) 0) (generateInfMaze (11,51)) stopCriteria))
