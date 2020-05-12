module Apps.GAMazeSolver.Run where

import Apps.GAMazeSolver.Interaction
import Apps.GAMazeSolver.Settings
import Apps.GAMazeSolver.GA
import GA
import System.Random
--------------------------------- The Entry Points -----------------------------
solution1:: IO()
solution1 = visualization [(1,10), (1,11)]

solution2 :: IO()
solution2 = visualization (initPopulation genePoints populationSize)

solution3 :: IO()
solution3 = ruAlgorithm

ruAlgorithm :: IO()
ruAlgorithm = visualization agents
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
                (mkStdGen 42)
