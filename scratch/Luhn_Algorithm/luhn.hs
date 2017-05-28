import Data.List
import Data.Char

mod10 n = if n > 9 then (n `mod` 10) + 1 else n
toIntPair ch n = (digitToInt ch, n)

luhn :: String -> Bool
luhn creditCard = 
  "0" `isSuffixOf` show (s1 + s2)
  where
    (odds, evens) = partition (\(ch,n) -> n `mod` 2 /= 0) $ zipWith toIntPair (reverse creditCard) [1..]
    s1 = sum $ fst $ unzip odds
    s2 = sum $ map mod10 $ map (*2) $ fst $ unzip evens
  
main :: IO ()
main = do
  let creditCards = ["2621195162335", "49927398716", "1234567812345670", "4485284720134093"] ++ ["49927398717", "1234567812345678"]
  print $ filter luhn creditCards
