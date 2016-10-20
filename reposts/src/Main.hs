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
type Reposts = Int
type Prizes = Int


numberOfSimulations::Int
numberOfSimulations = 10000

numberOfPrizes :: Int
numberOfPrizes = 7

simulateNtimes :: Simulations -> Reposts -> Prizes -> IO Winners
simulateNtimes n rep prizesNumber =
  concat <$> (sequence $ replicate n sim)
  where 
    sim = getNRandomNumbersInRange prizesNumber (1,rep)

getNRandomNumbersInRange :: Int -> (Int, Int) -> IO [Int]
getNRandomNumbersInRange n (start,stop) =
  sequence $ replicate n (randomRIO (start,stop))


computeProbability :: Simulations -> Reposts -> Reposts -> IO Double
computeProbability n reps extrarep = do
  winners <- simulateNtimes n (reps+extrarep) numberOfPrizes
  let
    extrarepList :: [Int]
    extrarepList = if extrarep == 0 then [] else [reps..(reps+extrarep)]
    occurences :: Int
    occurences =  foldr (\a acc ->
                          if (elem a extrarepList) then acc+1 else acc) 0 winners
    prob:: Double
    prob = (fromInteger (toInteger occurences))/(fromInteger (toInteger (n+extrarep)))
  return prob

main :: IO ()
main = do
   prob10 <- computeProbability numberOfSimulations 1000000 10
   prob100 <- computeProbability numberOfSimulations 1000000 100
   prob1000 <- computeProbability numberOfSimulations 1000000 1000
   putStrLn $ "Probability from " ++
     (show numberOfSimulations) ++ " simulations for the last 10 reposts of 1000010 reposts is " ++ (show prob10) 
   putStrLn $ "Probability from " ++
     (show numberOfSimulations) ++ " simulations for the last 100 reposts of 1000100 reposts is " ++ (show prob100)
   putStrLn $ "Probability from " ++
     (show numberOfSimulations) ++ " simulations for the last 1000 reposts of 1001000 reposts is " ++ (show prob1000)

