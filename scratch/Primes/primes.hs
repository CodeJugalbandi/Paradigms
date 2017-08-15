isPrime n = filter (\x -> n `mod` x == 0) [2..n] == [n]

primes n = filter isPrime [1..n]

main :: IO ()
main = do
  print $ primes 13
