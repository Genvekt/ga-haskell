module DataStructures.Maze where

-- | Maze elements
data Tile = Wall | Floor | Exit | FakeExit
 deriving (Show)

-- | Coordinates
type Coords = (Int, Int)

-- | Sequence of coordinates
type Path = [Coords]

-- | The maze to solve
type Maze = [[Tile]]
