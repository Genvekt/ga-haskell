module Main where
import Apps.GAMazeSolver.Run
import MazeHandler.Generator
import Graphics.Gloss
import Visualiser.Maze


main :: IO ()
--main = solution3
main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (scale 50 50 (drawBoard(generateMaze (21,21))))
