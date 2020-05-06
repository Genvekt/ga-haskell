module GA where
import System.Random
import Data.List
import Data.Maybe
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
  -> ([a] -> IO())  -- ^ Visualization function
  -> IO()           -- ^ Generation with best genes


-- | Base case only selected
geneticAlgorithm population _ percent _ 0 fit _ _ simulate= do
    let scored_parents = map (\x -> (fit x,x)) population
    let best_parents = takeBest scored_parents percent
    putStr("Best generation: ")
    putStr(show(best_parents))
    simulate (take 3(best_parents))


-- | General case, run more generations
geneticAlgorithm population prob_chrom percent_to_take pop_size generations fit cross mutate simulate= do
 probs <- randomList (length(population)) :: IO([Double])
 let scored_parents = map (\x -> (fit x,x)) population
 let new_population = map (mutate prob_chrom) (zip probs children)
 putStr("Generation: ")
 putStrLn(show(generations))
 putStr("Population score: ")
 putStrLn(show(scored_parents))
 putStr("Best ones: ")
 putStrLn(show(selected_parents))
 putStr("Children: ")
 putStrLn(show(children))
 putStr("To next generation: ")
 putStrLn(show(new_population))
 putStrLn("")
 geneticAlgorithm new_population prob_chrom percent_to_take pop_size (generations-1) fit cross mutate simulate
  where
   scored_parents = map (\x -> (fit x,x)) population
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


randomList :: Int -> IO([Double])
randomList 0 = return []
randomList n = do
 r  <- randomIO
 rs <- randomList (n-1)
 return (r:rs)
