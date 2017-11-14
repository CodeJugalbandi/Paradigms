toFizzBuzz :: Int -> String
toFizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3 == 0 = "Fizz"
  | n `mod` 5 == 0 = "Buzz"
  | otherwise = ""
  
  
fizzbuzz n = map toFizzBuzz [1..n]

main :: IO ()
main = do
  print $ fizzbuzz 20
  