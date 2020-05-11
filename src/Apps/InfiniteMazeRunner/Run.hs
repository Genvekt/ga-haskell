module Apps.InfiniteMazeRunner.Run where
import Apps.InfiniteMazeRunner.GA
import MazeHandler.Generator
import Apps.InfiniteMazeRunner.Settings
import Apps.InfiniteMazeRunner.Interaction
import GA
import System.Random


runAlgorithm :: (Int,Int) -> IO()
runAlgorithm size=  agentsInMaze size agents
  where
    (agents, _) = geneticAlgorithm
                (initPopulation genePoints populationSize)
                0.4
                70.0
                populationSize
                generations
                (evaluateHero (generateInfMaze size))
                crossGenes
                mutateGene
                (mkStdGen 33)
