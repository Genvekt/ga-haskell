# Modules
The project consists of next Modules

### Agent
Agent unites two types of agents:
 - **MazeSolver** - solves ordinary mazes
 - **InfMazeSolver** - runs in infinite mazes

### Data Structures
Consists of data structures relevant to **Maze** and to **Agen**.

### MazeHandler
Main module for maze based operations and functions
- **Generator** - utilities for maze creation
- **Searcher** - functions to retrieve info about Maz (size, tile at some coordinate and etc.)
- **Transformer** - various functions that somehow transform maze
- **DFS** - runs DFS for agent in maze to find possible moves

### Visualiser
Graphic utils to draw **Maze** parts, **Agent** or **System** of maze and agents

### Apps
Contains who applications for ordinary maze and infinite maze.
Each contains:
- **Interaction** - module for visualization of agents in maze.
- **GA** - Genetic Algorithm parts specific for each app
- **Run** - Runs GA and starts interaction with best population
- **Settings** - GA parameters
