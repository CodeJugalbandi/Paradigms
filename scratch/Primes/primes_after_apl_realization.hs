import Data.List.Grouping
import Data.List

primes n = indices
  where
    matrix = splitEvery n [if (j `mod` i == 0) then 1 else 0 | i <- [1..n], j <- [1..n]]
    primes = map sum $ transpose matrix
    indices = map (+1) (elemIndices 2 primes)
  
main :: IO ()
main = do
  print $ primes 13