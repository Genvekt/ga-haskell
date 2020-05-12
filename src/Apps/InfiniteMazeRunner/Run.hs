module Apps.InfiniteMazeRunner.Run where
import Apps.InfiniteMazeRunner.GA
import MazeHandler.Generator
import Apps.InfiniteMazeRunner.Settings
import Apps.InfiniteMazeRunner.Interaction
import GA
import System.Random


runAlgorithm :: (Int,Int) -> Int -> IO()
runAlgorithm size seed=  agentsInMaze size seed agents
  where
    (agents, _) = geneticAlgorithm
                (initPopulation genePoints populationSize)
                0.4
                70.0
                populationSize
                generations
                (evaluateHero (generateInfMaze size seed))
                crossGenes
                mutateGene
                (mkStdGen 33)
