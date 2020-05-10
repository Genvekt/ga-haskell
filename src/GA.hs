module GA where
import System.Random
import Data.List
import Data.Maybe
import Data.Monoid ((<>))
-------------------- General Genetic Algorithm ---------------------------------

-- | General Genetic Algorithm
-- | a == Gene in our case
geneticAlgorithm
  :: Show a => [a]           -- ^ Initial population
  -> Double        -- ^ The probability a chromosome of an individual is mutated
  -> Double        -- ^ The percentage of the population that goes to cross over
  -> Int           -- ^ Population size
  -> Int           -- ^ Number of generations
  -> (a -> Double) -- ^ Fitness function
  -> ([a] -> [a])  -- ^ Crossover function
  -> (Double->(Double,a) -> a)      -- ^ Mutation function
  -> StdGen
  -> ([a], StdGen) -- ^ Generation with best genes


-- | Base case only selected
geneticAlgorithm population _ percent _ 0 fit _ _ gen = (best_parents, gen)
    where
      scored_parents = map (\x -> (fit x,x)) population
      best_parents = takeBest scored_parents percent


-- | General case, run more generations
geneticAlgorithm population prob_chrom percent_to_take pop_size generations fit cross mutate generator=
 geneticAlgorithm new_population prob_chrom percent_to_take pop_size (generations-1) fit cross mutate newGen
  where
   (probs, newGen) = randomList (length(population)) generator
   scored_parents = map (\x -> (fit x,x)) population
   new_population = map (mutate prob_chrom) (zip probs children)
   children = take pop_size (cross selected_parents)
   selected_parents = takeBest scored_parents percent_to_take


-- | Select the fittest genes from population
takeBest
 :: [(Double,a)]      -- ^ Genes with scores to sort
 -> Double            -- ^ The percentage of the population that goes to cross over
 -> [a]               -- ^ Best genes
takeBest population percent_to_take = selected_parents
  where
   selected_parents = map (\(score,x) -> x) selected
   selected = take select_num sorted_parents
   sorted_parents = sortOn fst population
   select_num = round (fromIntegral(current_size) * percent_to_take /100.0)
   current_size = length(population)


randomList :: Int -> StdGen -> ([Double], StdGen)
randomList 0 gen = ([], gen)
randomList n gen = ([random_num]<>random_list, newGen)
  where
   (random_num, nextGen) = randomR (0.0, 1.0) gen
   (random_list, newGen) = randomList (n-1) nextGen
