module MazeHandler.Generator where


import System.Random
import Data.List
import DataStructures.Maze


randomList :: Int -> IO([Double])
randomList 0 = return []
randomList n = do
  r  <- randomIO
  rs <- randomList (n-1)
  return (r:rs)
