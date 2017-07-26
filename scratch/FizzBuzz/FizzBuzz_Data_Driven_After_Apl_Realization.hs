import Data.List (isInfixOf)
import Data.Digits (unDigits)

fizzBuzz :: Int -> [String]
fizzBuzz n = take n result
  where
    -- Part 0
    -- threes = cycle [0, 0, 1]
    -- fives  = cycle [0, 0, 0, 0, 1]
    -- fizzBuzz = ["", "Fizz", "Buzz", "FizzBuzz"]
    -- result = map (fizzBuzz!!) $ zipWith (\x y -> unDigits 2 [x,y]) fives threes
    
    -- Part 1
    threes = cycle [0, 0, 1]
    fives  = cycle [0, 0, 0, 0, 1]
    fizzBuzz = ["", "Fizz", "Buzz", "FizzBuzz"]
    select = map (fizzBuzz!!) $ zipWith (\x y -> unDigits 2 [x,y]) fives threes
    allIndices = map show [1..n]
    result = zipWith (\i d -> if (d == "") then i else d) allIndices select
    
    -- Part 2
    -- threes = cycle [0, 0, 1]
    -- fives  = cycle [0, 0, 0, 0, 1]
    -- fizzBuzz = ["", "Fizz", "Buzz", "FizzBuzz"]
    -- select = map (fizzBuzz!!) $ zipWith (\x y -> unDigits 2 [x,y]) fives threes
    -- allIndices = map (\i -> if (isInfixOf "3" i) then "Lucky" else i) $ map show [1..n]
    -- result = zipWith (\i d -> if (d == "" || i == "Lucky") then i else d) allIndices select
    

main :: IO ()
main = do
  print $ fizzBuzz 20
