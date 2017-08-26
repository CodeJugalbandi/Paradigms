-- Very slow prime 
-- isPrime n = filter (\x -> n `mod` x == 0) [2..n] == [n]

-- Highly efficient prime 
isPrime n = and [n `mod` x > 0 | x <- takeWhile ((<=n).(^2)) (2:[3,5..n])]

primes n = filter isPrime [1..n]

main :: IO ()
main = do
  -- print $ primes 13
  -- print $ primes 130
  -- print $ primes 1300
  print $ primes 13000
