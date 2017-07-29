import Data.List (isInfixOf)
import Data.Digits (unDigits)
import Data.Time (getCurrentTime, diffUTCTime)


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
    base2 = unDigits 2
    result = zipWith3 (\f t i -> if(base2 [f,t] == 0) then show i else fizzBuzz !! (base2 [f,t])) fives threes [1..]

    -- Part 2
    -- threes = cycle [0, 0, 1]
    -- fives  = cycle [0, 0, 0, 0, 1]
    -- fizzBuzz = ["", "Fizz", "Buzz", "FizzBuzz", "Lucky"]
    -- indexContains3 n = isInfixOf "3" $ show n
    -- indices = zipWith3 (\f t i -> if (indexContains3 i) then 4 else (unDigits 2 [f,t])) fives threes [1..]
    -- result = zipWith (\idx n -> if (idx == 0) then show n else fizzBuzz!!idx) indices [1..]

main :: IO ()
main = do
  start <- getCurrentTime
  print $ fizzBuzz 2000
  stop <- getCurrentTime  
  print $ diffUTCTime stop start  
  print "DONE"