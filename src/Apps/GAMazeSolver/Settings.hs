module Apps.GAMazeSolver.Settings where
import DataStructures.Maze
--------------------------------- Settings -------------------------------------
-- | The maze that hero solves now
currentMaze :: Maze
currentMaze = testMaze3

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

-- | Test maze #1
testMaze1:: Maze
testMaze1 = [
 [w,w,w,w,w,w,w,w,w,w,w],
 [w,o,o,o,o,o,o,o,o,o,w],
 [w,o,w,w,w,w,w,w,w,w,w],
 [w,o,o,o,o,o,w,o,o,o,w],
 [w,w,w,o,w,o,w,o,w,o,w],
 [w,o,o,o,w,o,w,o,w,o,w],
 [w,o,w,w,w,w,w,o,w,o,w],
 [w,o,w,o,o,o,o,o,w,o,w],
 [w,o,w,o,w,w,w,w,w,o,w],
 [w,o,o,o,w,x,o,o,o,o,w],
 [w,w,w,w,w,w,w,w,w,w,w]]
  where
   w = Wall
   o = Floor
   x = Exit

-- | Test maze #2
testMaze2 :: Maze
testMaze2 = [
 [w,w,w,w,w,w,w,w,w,w,w],
 [w,o,o,o,o,o,o,o,w,o,w],
 [w,o,w,w,w,w,w,o,w,o,w],
 [w,o,o,o,w,o,o,o,o,o,w],
 [w,o,w,o,w,w,w,w,w,w,w],
 [w,o,w,o,o,o,o,o,o,o,w],
 [w,o,w,w,w,w,w,w,w,o,w],
 [w,o,w,o,w,o,o,o,w,o,w],
 [w,o,w,o,w,o,w,o,w,o,w],
 [w,o,o,o,w,x,w,o,o,o,w],
 [w,w,w,w,w,w,w,w,w,w,w]]

  where
   w = Wall
   o = Floor
   x = Exit

testMaze3 :: Maze
testMaze3 = [
 [w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w],
 [w,o,o,o,o,o,o,o,o,o,o,o,w,o,o,o,o,o,w,o,w],
 [w,o,w,w,w,w,w,w,w,w,w,o,w,o,w,o,w,o,w,o,w],
 [w,o,w,o,o,o,o,o,o,o,o,o,w,o,w,o,w,o,o,o,w],
 [w,o,w,o,w,w,w,w,w,w,w,w,w,o,w,o,w,w,w,w,w],
 [w,o,w,o,w,o,o,o,w,o,o,o,o,o,w,o,w,o,o,o,w],
 [w,o,w,o,w,o,w,o,w,o,w,w,w,w,w,o,w,o,w,o,w],
 [w,o,w,o,o,o,w,o,o,o,o,o,w,o,o,o,w,o,w,o,w],
 [w,o,w,w,w,w,w,w,w,w,w,w,w,o,w,o,w,o,w,o,w],
 [w,o,o,o,o,o,o,o,o,o,w,x,o,o,w,o,o,o,w,o,w],
 [w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w,w]]

  where
   w = Wall
   o = Floor
   x = Exit
