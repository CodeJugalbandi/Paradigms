import Data.List (isInfixOf)
import Data.Time (getCurrentTime, diffUTCTime)

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
    indices = map show [1..]
    select = \i t f -> if (t == "" && f == "") then i else t ++ f
    result = zipWith3 select indices threes fives
    -- result = zipWith3 (\i t f -> max i $ t ++ f) indices threes fives

    -- Part 2
    -- threes = cycle ["", "", "Fizz"]
    -- fives  = cycle ["", "", "", "", "Buzz"]
    -- luckyOrIndex = \i -> if (isInfixOf "3" $ show i) then "Lucky" else show i
    -- select = map luckyOrIndex [1..]
    -- result = zipWith3 (\i t f -> max i $ t ++ f) select threes fives

main :: IO ()
main = do
  start <- getCurrentTime
  print $ fizzBuzz 2000
  stop <- getCurrentTime  
  print $ diffUTCTime stop start  
  print "DONE"