module MazeHandler.Generator where


import System.Random
import Data.List
import Data.Monoid ((<>))

import DataStructures.Maze
import MazeHandler.Searcher
import MazeHandler.Transformer


randomList :: Int -> IO([Double])
randomList 0 = return []
randomList n = do
  r  <- randomIO
  rs <- randomList (n-1)
  return (r:rs)

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

generateMaze :: (Int,Int)-> StdGen -> (Maze, StdGen)
generateMaze size generator = (unmarkedMaze, updGen)
  where
    initialGrid = createGrid size
    (maze, updGen) = dfsStep (2,2) markedGrid generator
    markedGrid = map (map (\x -> (x, False))) initialGrid
    unmarkedMaze = map (map (\(x,_) ->  x)) maze

dfsStep :: Coords -> [[(Tile,Bool)]] -> StdGen -> ([[(Tile,Bool)]], StdGen)
dfsStep (i,j) board generator = runDFS
  -- get all unvisited neighbours
  where
    neighbours = getUnvisited (i-2,j) board <> getUnvisited (i+2,j) board<>
                 getUnvisited (i,j+2) board <> getUnvisited (i,j-2) board
    (nextRandomCell, updGen) = randomR (1, length neighbours) generator
    getUnvisited coords grid
     | isUnvisited (whatCell grid coords) = [coords]
     | otherwise = []

    isUnvisited Nothing = False
    isUnvisited (Just (_,True)) = False
    isUnvisited (Just (_,False)) = True

    updatedBoard = changeCellAt board (i,j) (Floor, True)
    nextCell (Just cellCoords) =  cellCoords
    nextCell Nothing = (i,j)
    runDFS
     | length neighbours > 0 = dfsStep (i,j) boardAfterDFS newGen
     | otherwise = (updatedBoard, generator)
      where
        mean (x1,y1) (x2,y2) = ((x1+x2)`div` 2,(y1+y2)`div` 2)
        neighbour = nextCell (lookup nextRandomCell (zip [1..] neighbours))
        wallToRemove = mean neighbour (i,j)
        boarWithoutWall = changeCellAt updatedBoard wallToRemove (Floor, True)
        (boardAfterDFS, newGen) = dfsStep neighbour boarWithoutWall updGen

length' :: [a] -> Int
length' [] = 0
length' xs = sum [1 | _ <- xs]
