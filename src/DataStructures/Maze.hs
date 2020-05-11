module DataStructures.Maze where
import System.Random

-- | Maze elements
data Tile = Wall | Floor | Exit | FakeExit
 deriving (Show)

-- | Coordinates
type Coords = (Int, Int)

-- | Sequence of coordinates
type Path = [Coords]

-- | The maze to solve
type Maze = [[Tile]]

-- | Timer of inf maze update
type Timer = Int

-- | The part of left maze to delete
type Shift = Int

-- | Infinite Maze with two parts, shift from the left and random generator
data InfMaze = InfMaze Maze Maze Int StdGen Timer
