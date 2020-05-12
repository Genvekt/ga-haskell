# haskell-ga
This project is based on Genetic Algorithm for agents walking in mazes. It consists of 2 parts - Ordinary maze and Infinite maze as environment.

# Run project
project may be built using stack and the Glasgow Haskell Compiler.

Program itself has next parameters which may be specified:
```
1. "inf"   - Run in infinite maze
   "ord"   - Run in ordinary maze

2. "ga"    - Run Genetic Algorithm before visualization
   "no-ga" - Run without Genetic Algorithm

3. seed    - Number which is used for maze generation, integer

4. height  - Height of the maze, integer
4. width   - Width of the maze, integer

```
If no-ga is choosed, user will be asked for agent parameters.

**Example of run command**
```
./ga-haskell inf no-ga 3 21 51^C
```
# Maze Structure
Maze may consist of several types of tiles:
 - **Floor** - tiles on which agent may step on
 - **Wall** - tiles that play role of the obstacles
 - **Exit** - tile that represents exit, may not be present in maze (in infinite one definitely)

# Maze Generation
Maze is generated using DFT based algorithm from cell (2,2). **It is important that maze height and width are odd numbers.** The main property of this algorithm is that it always has path from (2,2) to any cell in the maze.
# Agent parameters

So far agents has next parameters:

- **Vision** - the max cells number that agent may see in all 4 directions
around him.

- **Memory** - the number of steps it remembers.

The movement of agent id determined by his vision and memory.

It firstly determines the Floor tiles that lies on the edge of his vision, as they are possible ways out of the maze. Secondly, agent choose one of 4 directions leading to at least 1 cell specified in first step using its memory, based on idea of moving to sell that it remembers worse of all.
