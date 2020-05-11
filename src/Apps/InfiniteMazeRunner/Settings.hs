module Apps.InfiniteMazeRunner.Settings where
import DataStructures.Maze
-- | The max sum of hromosomes in gene
genePoints :: Int
genePoints = 14

-- | Number of ingividual genes in each population
populationSize :: Int
populationSize = 10

-- | The number of populations to compute
generations :: Int
generations = 20
