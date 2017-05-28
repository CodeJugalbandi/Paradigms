import Data.List
import Data.Char

sumDigits :: Int -> Int
sumDigits n 
  | n > 9 = sum $ map digitToInt $ show n
  | otherwise = n

luhn :: String -> Bool
luhn creditCard = 
  "0" `isSuffixOf` show (s1 + s2)
  where
    (odds, evens) = partition (\(ch,n) -> n `mod` 2 /= 0) (zip (reverse creditCard) [1..])
    toInt = map (\(ch, n) -> (digitToInt ch))
    s1 = sum $ toInt odds
    s2 = sum $ map sumDigits $ map (*2) $ toInt evens
  
main :: IO ()
main = do
  let validNumbers = ["2621195162335", "49927398716", "1234567812345670", "4485284720134093"]
  let invalidNumbers = ["49927398717", "1234567812345678"]
  let creditCards = validNumbers ++ invalidNumbers
  print $ filter luhn creditCards
