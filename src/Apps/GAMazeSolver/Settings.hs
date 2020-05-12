module Apps.GAMazeSolver.Settings where
import DataStructures.Maze
--------------------------------- Settings -------------------------------------

-- | Initial coordinations for each gene
startCoords :: Coords
startCoords = (2,6)

-- | Number of ingividual genes in each population
populationSize :: Int
populationSize = 10

-- | The maximum number of steps the hero may make
heroHealth :: Int
heroHealth = 20

-- | The max sum of hromosomes in gene
genePoints :: Int
genePoints = 7

-- | The number of populations to compute
generations :: Int
generations = 20
