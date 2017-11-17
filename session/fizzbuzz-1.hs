toFizzBuzz :: Int -> String
toFizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3 == 0 = "Fizz"
  | n `mod` 5 == 0 = "Buzz"
  | otherwise = ""
  

main :: IO ()
main = do
  print $ toFizzBuzz 5
  