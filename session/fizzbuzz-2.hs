import Data.Digits

fizzbuzz n = take n result
    where
      threes = cycle [0, 0, 1]
      fives = cycle [0, 0, 0, 0, 1]
      indices = map show [1..n]
      text = ["", "Fizz", "Buzz", "FizzBuzz"]
      base2 = unDigits 2
      result = zipWith3 (\t f i -> if(base2 [f, t] == 0) then i else text!!base2 [f, t]) threes fives indices

main :: IO ()
main = do
  print $ fizzbuzz 20
  