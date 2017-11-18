import Data.Monoid
 
fizzbuzz = max
       <$> show
       <*> "fizz" `when` divisibleBy 3
       <>  "buzz" `when` divisibleBy 5
       <>  "bleep" `when` divisibleBy 7
  where
    when m p x = if p x then m else mempty
    divisibleBy n x = x `mod` n == 0
 
main = mapM_ (print . fizzbuzz) [1..20]  