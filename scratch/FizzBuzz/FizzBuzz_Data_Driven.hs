import Data.Char
import Data.List

fizzBuzz :: Int -> [String]
fizzBuzz n = take n result
  where
    -- Part 0
    -- threes = cycle ["", "", "Fizz"]
    -- fives  = cycle ["", "", "", "", "Buzz"]
    -- result = zipWith (++) threes fives
    
    -- Part 1
    threes = cycle ["", "", "Fizz"]
    fives  = cycle ["", "", "", "", "Buzz"]
    indices = map show [1..n]
    -- result = zipWith3 (\i t f -> max i $ t ++ f) indices threes fives
    result = zipWith3 (\i t f -> if (t == "" && f == "") then i else t ++ f) indices threes fives
    
    -- Part 2
    -- threes = cycle ["", "", "Fizz"]
    -- fives  = cycle ["", "", "", "", "Buzz"]
    -- luckies = cycle ["Lucky"]
    -- indices = zipWith (\x y -> if (isInfixOf "3" x) then y else x) (map show [1..n]) luckies
    -- result = zipWith3 (\i t f -> max i $ t ++ f) indices threes fives

main :: IO ()
main = do
  print $ fizzBuzz 20
  
  