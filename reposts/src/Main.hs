module Main where
import System.Random
-- 2. Calculate probability of winning using simulation {reposts}

-- There's a contest going on in a Russian social network: seven prizes will be given to seven randomly chosen people among
-- those who have reposted a certain post. (There are actually 100 prizes, but the other 93 suck, so we'll ignore them.)
-- There are already ~1000000 reposts. My sister wonders: what's the probability of her winning at least one prize
-- (out of those seve) if she reposts the post 10 times (from different accounts)? What about 100 times? 1000 times?

-- Calculate the answer by running a simulation some number of times (for instance, 10000 times).
-- You can use System.Random or some other random library (e.g. Data.Random).

-- If you're not good at probabilistic simulations, here's a hint.

type Winners = [Int]
type Simulations = Int
type Repost = Int
type Reposts = [Repost]
type Prizes = Int


numberOfSimulations::Int
numberOfSimulations = 10000

numberOfPrizes :: Int
numberOfPrizes = 7

simulateNtimes :: Simulations -> Reposts -> Prizes -> Winners
simulateNtimes sim rep pri =

  where
    sim_result = randomRs (1,rep)
    getWinnersIndices = take pri sim_result
    getWinners = fmap (rep!!) getWinnersIndices

computeProbability :: Simulations -> Reposts -> Repost -> Double
computeProbability sim reps rep =
  (fromInteger (toInteger occurences))/(fromInteger (toInteger sim))
  where
    occurences = length $ filter (rep==) sim_results
    sim_results = simulateNtimes sim reps numberOfPrizes
  

main :: IO ()
main = do
  putStrLn (show $ computeProbability numberOfSimulations [1 .. numberOfSimulations] 21)

