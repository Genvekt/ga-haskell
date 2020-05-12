module MazeHandler.Generator where


import System.Random
import Data.List
import Data.Monoid ((<>))

import DataStructures.Maze
import MazeHandler.Searcher
import MazeHandler.Transformer


-- Grid of floors surrounded by walls
createGrid ::(Int, Int) -> Maze
createGrid (height, width)
 | height <= 0 = []
 | otherwise = [gridRow (height, width)] <> createGrid (height-1, width)

gridRow :: (Int, Int) -> [Tile]
gridRow (height, width)
 | width <= 0 = []
 | height `mod` 2 == 0 = alternatingRow
 | otherwise = [Wall] <> gridRow (height, width-1)
   where
     alternatingRow
      | width `mod` 2 == 0 = [Floor] <> gridRow (height, width-1)
      | otherwise = [Wall] <> gridRow (height, width-1)

-- | Generate one maze
generateMaze
 ::(Int,Int)       -- ^ Height and Width of the maze
 -> StdGen         -- ^ Random generator
 -> (Maze, StdGen) -- ^ Resulted maze and updated Generator
generateMaze size generator = (unmarkedMaze, updGen)
  where
    initialGrid = createGrid size
    (maze, updGen) = dfsStep (2,2) markedGrid generator
    markedGrid = map (map (\x -> (x, False))) initialGrid
    unmarkedMaze = map (map (\(x,_) ->  x)) maze

-- | One single step of DFT algorithm for maze generation
dfsStep
:: Coords                    -- ^ Current position
-> [[(Tile,Bool)]]           -- ^ Maze where each tile is market as visited or not
-> StdGen                    -- ^ Random Generator
-> ([[(Tile,Bool)]], StdGen) -- ^ Resulted maze after one step and updated Generator
dfsStep (i,j) board generator = runDFS
  where
    neighbours = getUnvisited (i-2,j) board <> getUnvisited (i+2,j) board<>      -- ^ Get all unvisited neighbours
                 getUnvisited (i,j+2) board <> getUnvisited (i,j-2) board
    (nextRandomCell, updGen) = randomR (1, length neighbours) generator          -- ^ Choose random unvisited neighbour
    getUnvisited coords grid
     | isUnvisited (whatCell grid coords) = [coords]
     | otherwise = []

    isUnvisited Nothing = False
    isUnvisited (Just (_,True)) = False
    isUnvisited (Just (_,False)) = True

    updatedBoard = changeCellAt board (i,j) (Floor, True)                        -- ^ Mark current position as visited
    nextCell (Just cellCoords) =  cellCoords
    nextCell Nothing = (i,j)
    runDFS                                                                       -- ^ Result of next DFS step
     | length neighbours > 0 = dfsStep (i,j) boardAfterDFS newGen                -- ^ Case when there are unvisited neighbours
     | otherwise = (updatedBoard, generator)                                     -- ^ Case when there is no unvisited neighbours
      where
        mean (x1,y1) (x2,y2) = ((x1+x2)`div` 2,(y1+y2)`div` 2)
        neighbour = nextCell (lookup nextRandomCell (zip [1..] neighbours))
        wallToRemove = mean neighbour (i,j)
        boarWithoutWall = changeCellAt updatedBoard wallToRemove (Floor, True)   -- ^ remove wall between selected neighbour and
        (boardAfterDFS, newGen) = dfsStep neighbour boarWithoutWall updGen       -- ^ current cell

-- | Generate InfMaze
generateInfMaze :: (Int,Int)-> Int -> InfMaze
generateInfMaze size seed = InfMaze maze1 maze2 0 generator2 0
  where
    (maze1, generator1) = generateMaze size (mkStdGen seed)
    (maze2, generator2) = generateMaze size generator1


-- | Merge two mazes to one with shift
makeMaze :: Maze -> Maze -> Shift -> Maze
makeMaze maze1 maze2 shift= union
 where
   (_, width) = mazeShape maze1
   croppedPart1 = map (take (width-1)) maze1
   uppendedPart1 = map uppend (zip [1..] croppedPart1)
   part1 = map (drop shift) uppendedPart1
   part2 = map ((take shift).(drop 1))  maze2
   uppend (index, row)
    | index `mod` 2 == 0 = row <> [Floor]
    | otherwise = row <> [Wall]
   union = map (\(row1, row2) -> [Wall] <> row1 <> row2 <> [Wall]) (zip part1 part2)
