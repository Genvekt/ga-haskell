module Apps.GAMazeSolver.Run where

import Apps.GAMazeSolver.Interaction
import Apps.GAMazeSolver.Settings
import Apps.GAMazeSolver.GA
import MazeHandler.Generator
import GA
import System.Random
import MazeHandler.Generator
--------------------------------- The Entry Points -----------------------------

runAlgorithm :: (Int, Int) -> Int -> IO()
runAlgorithm size seed= visualization size seed agents
  where
    (agents, _) = geneticAlgorithm
                (initPopulation genePoints populationSize)
                0.4
                70.0
                populationSize
                generations
                (evaluateHero currentMaze)
                crossGenes
                mutateGene
                (mkStdGen seed)
    (currentMaze,_) = generateMaze size (mkStdGen seed)
