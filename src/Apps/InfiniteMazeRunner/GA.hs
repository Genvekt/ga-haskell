module Apps.InfiniteMazeRunner.GA where

import Apps.InfiniteMazeRunner.Settings

import Data.List
import Data.Maybe

import DataStructures.Maze
import DataStructures.Agent

import Agent.InfMazeSolver

------------------- Initial Population Builder ---------------------------------
-- | The starting population
initPopulation
 :: Int         -- ^ The number of genetical points
 -> Int         -- ^ The size of the population
 -> [Gene]      -- ^ Generated opulation
initPopulation amount size = take size (map makeGene [0,1..])
 where
  makeGene x = (x, amount-x)

------------------------ Fit Function ------------------------------------------
-- | The criteria when hero stops
stopCriteria :: Hero -> Bool
stopCriteria (Hero _ _ (_, y) lives )
 | lives > 1000 = True
 | y > 0 && lives <=100 = False
 | otherwise = True

-- | Write hero walk through maze and return how many steps it makes before death
evaluateHero :: InfMaze ->(Gene -> Double)
evaluateHero maze gene = -1 * (fromIntegral heroHealth)
  where
    (Hero _ _ _ heroHealth) =
       runInInfMaze (Hero gene [] (2,2) 0) maze stopCriteria

------------------------ Cross Over Function -----------------------------------

-- | Generate new genes from given ones                                           -- ^ Takes n genes
crossGenes :: [Gene] -> [Gene]                                                   -- ^ Returns n new crossed by mean genes
crossGenes [] = []
crossGenes [(gf1, gs1)] = [( mean gf1 gs1, mean gf1 (mean gf1 gs1))]
crossGenes [(gf1, gs1), (gf2, gs2)] = [(mean gf1 gf2, mean gs1 gs2)]
                                    ++ crossGenes([(gf2, gs2)])

crossGenes (gene1:gene2:genes)      = [(mean f1 f2, mean s1 s2)]
                                    ++ [(mean f1 f3, mean s1 s3)]
                                    ++ crossGenes([gene2] ++ genes)
  where
    f1 = fst gene1
    f2 = fst gene2
    f3 = fst (head genes)
    s1 = snd gene1
    s2 = snd gene2
    s3 = snd (head genes)

-- | Mean value of two integers x and y
mean :: Int -> Int -> Int
mean x y = round( fromIntegral(x+y) / 2)

------------------------- Mutation Function ------------------------------------

-- | Mutate each element of gene with given probability
mutateGene
 :: Double         -- ^ The treshold for mutation to happen
 -> (Double,Gene)  -- ^ Gene to mutate with its probability to mutate
 -> Gene           -- ^ Mutated gene
mutateGene  treshold (prob,(vision,memory))
  | treshold > prob = (vision,memory)                                            -- ^ No mutation happens
  | prob < sub_treshold = update_gene_on growth
  | otherwise = update_gene_on (-growth)
    where
      total_score = genePoints
      growth = 1
      sub_treshold = (1 - treshold)/2 + treshold
      update_gene_on value
       | vision + value > total_score = (total_score, 0)
       | vision + value < 0 = (0, total_score)
       | otherwise = (vision + value, total_score - (vision + value))
