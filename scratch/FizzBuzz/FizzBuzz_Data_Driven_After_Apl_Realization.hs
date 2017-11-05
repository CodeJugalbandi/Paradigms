import Data.Digits (digits, unDigits)
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
    -- threes = cycle [0, 0, 1]
    -- fives  = cycle [0, 0, 0, 0, 1]
    -- indices = [1..]
    -- fizzBuzz = ["", "Fizz", "Buzz", "FizzBuzz"]
    -- base2 = unDigits 2
    -- select = \f t i -> if(base2 [f,t] == 0) then show i else fizzBuzz !! (base2 [f,t])
    -- result = zipWith3 select fives threes indices

    -- Part 2
    threes = cycle [0, 0, 1]
    fives  = cycle [0, 0, 0, 0, 1]
    fizzBuzz = ["", "Fizz", "Buzz", "FizzBuzz", "Lucky"]
    contains3 n = elem 3 $ digits 10 n
    select = \f t i -> if (contains3 i) then 4 else (unDigits 2 [f,t])
    indices = zipWith3 select fives threes [1..]
    result = zipWith (\idx n -> if (idx == 0) then show n else fizzBuzz!!idx) indices [1..]

main :: IO ()
main = do
  start <- getCurrentTime
  print $ fizzBuzz 2000
  stop <- getCurrentTime  
  print $ diffUTCTime stop start  
  print "DONE"