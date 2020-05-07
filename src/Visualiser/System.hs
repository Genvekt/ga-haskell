module Visualiser.System where

import Graphics.Gloss
import Data.Monoid ((<>))

import DataStructures.Maze
import DataStructures.Agent

import MazeHandler.Searcher
import Visualiser.Maze
import Visualiser.Agent
-----------------------------  Draw Functions ----------------------------------

drawSystem:: Maze -> [Hero] -> Picture
drawSystem maze heros =
 scale size size (translate (-i/2) (-j/2) ( drawBoard maze  <> mconcat (map drawHero heros) ))
  where
   (height,width) = mazeShape maze
   i = fromIntegral height
   j = fromIntegral width
   size
    | i > j = j/i *50
    | otherwise = i/j *50
