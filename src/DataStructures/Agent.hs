module DataStructures.Agent where
import DataStructures.Maze
-- | Gene to modify (vision and memory)
type Gene = (Int,Int)

-- | The state of the gene in the simulation
data Hero = Hero
                 Gene    -- | The genom of the hero with its vision and memory params
                 Path    -- | The memorized past path
                 Coords  -- | The position in maze
                 Int     -- | Moves left (Health)
