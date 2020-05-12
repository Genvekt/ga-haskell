module Main where

import System.Environment
-- import Apps.GAMazeSolver.Run
--import Apps.GAMazeSolver.Run
import Agent.InfMazeSolver
import Apps.InfiniteMazeRunner.GA
import MazeHandler.Generator
import DataStructures.Agent

import Apps.InfiniteMazeRunner.Run
import Apps.InfiniteMazeRunner.Interaction

import Apps.GAMazeSolver.Run
import Apps.GAMazeSolver.Interaction




main :: IO ()
main = do
  args <- getArgs
  dispatch args
--main = putStrLn ( show (runInInfMaze (Hero (0,0) [] (2,2) 0) (generateInfMaze (11,51)) stopCriteria))

dispatch :: [String] -> IO ()
dispatch ["inf", "ga", seed, h, w] = do
  let hNum = (read h :: Int)
  let wNum = (read w :: Int)
  let seedNum = (read seed :: Int)
  Apps.InfiniteMazeRunner.Run.runAlgorithm (hNum, wNum) seedNum

dispatch ["inf", "no-ga", seed, h, w] =   do
  putStrLn("What is agent vision? Integer from 1.")
  vision <- getLine
  putStrLn("What is agent memory? Integer from 1.")
  memory <- getLine
  let v = (read vision :: Int)
  let m = (read memory :: Int)
  let hNum = (read h :: Int)
  let wNum = (read w :: Int)
  let seedNum = (read seed :: Int)
  let gene = [(v,m)]
  Apps.InfiniteMazeRunner.Interaction.agentsInMaze (hNum, wNum) seedNum gene

dispatch ["ord", "ga", seed, h, w] =   do
  let hNum = (read h :: Int)
  let wNum = (read w :: Int)
  let seedNum = (read seed :: Int)
  Apps.GAMazeSolver.Run.runAlgorithm (hNum, wNum) seedNum

dispatch ["ord", "no-ga", seed, h, w] =   do
  putStrLn("What is agent vision? Integer from 1.")
  vision <- getLine
  putStrLn("What is agent memory? Integer from 1.")
  memory <- getLine
  let v = (read vision :: Int)
  let m = (read memory :: Int)
  let hNum = (read h :: Int)
  let wNum = (read w :: Int)
  let seedNum = (read seed :: Int)
  let gene = [(v,m)]
  Apps.GAMazeSolver.Interaction.visualization (hNum, wNum) seedNum gene

