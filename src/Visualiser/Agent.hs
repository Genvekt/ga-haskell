module Visualiser.Agent where

import Graphics.Gloss
import Data.Monoid ((<>))

import DataStructures.Agent
import DataStructures.Maze

import Visualiser.Maze


drawHero :: Hero -> Picture
drawHero (Hero (vision, _) history (x,y) _) =
 (color green (drawCellAt y x Exit)) <>
 drawMemory history <>
 drawVision (x,y) vision


drawMemory :: [Coords] -> Picture
drawMemory memory = pictures (map showCell agedMoves)
 where
  agedMoves = zip [1..] memory
  showCell (age, (x,y)) =(color ( withAlpha (1 -age/ fullMemory) yellow) (drawCellAt y x Wall))
  fullMemory = (fromIntegral (length(memory)))


drawVision :: Coords -> Int -> Picture
drawVision (x,y) radius = (translate j i (color (withAlpha 0.3 (blue)) (rectangleSolid r 3))) <>
  (translate j i (color (withAlpha 0.3 (blue)) (rectangleSolid 3 r)))
 where
  i = fromIntegral x
  j = fromIntegral y
  r = fromIntegral radius *2 +1
