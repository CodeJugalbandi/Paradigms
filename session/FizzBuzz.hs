import Data.Digits

fizzBuzz :: Int -> [String]
fizzBuzz n = take n result
  where
    threes = cycle [0, 0, 1]
    fives = cycle [0, 0, 0, 0, 1]
    indices = map show [1..n]
    message = ["", "Fizz", "Buzz", "FizzBuzz"]
    base2 = unDigits 2
    select = (\f t i -> if (base2 [f,t] == 0) then i else message !! base2 [f,t])
    result = zipWith3 select fives threes indices

main :: IO ()
main = do
  print $ fizzBuzz 20
