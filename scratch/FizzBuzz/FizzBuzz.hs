import Data.List (isInfixOf)

-- Part 0
fizzBuzz :: Int -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3 == 0 = "Fizz"
  | n `mod` 5 == 0 = "Buzz"
  | otherwise =  ""

-- Part 1
-- fizzBuzz :: Int -> String
-- fizzBuzz n
--   | n `mod` 15 == 0 = "FizzBuzz"
--   | n `mod` 3 == 0 = "Fizz"
--   | n `mod` 5 == 0 = "Buzz"
--   | otherwise = show n
  
-- Part 2
-- fizzBuzz :: Int -> String
-- fizzBuzz n
--   | isInfixOf "3" (show n) = "Lucky"
--   | n `mod` 15 == 0 = "FizzBuzz"
--   | n `mod` 3 == 0 = "Fizz"
--   | n `mod` 5 == 0 = "Buzz"
--   | otherwise = show n

main :: IO ()
main = do
  print $ map fizzBuzz [1..20]
  