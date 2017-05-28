import Data.List
import Data.Char

doubledSum :: Int -> Int
doubledSum n = if doubled > 9
  then (doubled `mod` 10) + 1
  else doubled
  where
    doubled = 2 * n

luhn :: String -> Bool
luhn creditCard = 
  "0" `isSuffixOf` show (s1 + s2)
  where
    (odds, evens) = partition (\(ch,n) -> n `mod` 2 /= 0) (zip (reverse creditCard) [1..])
    toInt = map (\(ch, n) -> (digitToInt ch))
    s1 = sum $ toInt odds
    s2 = sum $ map doubledSum $ toInt evens
  
main :: IO ()
main = do
  let creditCards = ["2621195162335", "49927398716", "1234567812345670", "4485284720134093"] ++ ["49927398717", "1234567812345678"]
  print $ filter luhn creditCards
