import Data.List
import Data.Char
import Data.Digits

luhn :: String -> Bool
luhn creditCard = 
      total `mod` 10 == 0
  where
    reversedDigits = map digitToInt $ reverse creditCard
    oddsAndDoubledEvens = zipWith (*) reversedDigits (take (length creditCard) $ cycle [1,2])
    base10 = map (digits 10) oddsAndDoubledEvens
    total = sum $ map sum base10

main :: IO ()
main = do
  let creditCards = ["2621195162335", "49927398716", "1234567812345670", "4485284720134093"] ++ ["49927398717", "1234567812345678"]
  print $ filter luhn creditCards
