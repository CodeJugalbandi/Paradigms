import Data.Char

type Parser a = String -> [(a, String)]

-- primitive parsers
success :: Char -> Parser Char
success ch = \input -> [(ch ,input)]

failure :: Parser Char
failure = \input -> []

-- consumes first character for non-empty string, fails
-- otherwise.
item :: Parser Char
item = \input -> case input of
  [] -> []
  (ch:chs) -> [(ch, chs)]
  
-- parser combinators
(~>) :: Parser a -> Parser b -> Parser (a,b)
p ~> q = \input -> [((v,w), input'') | (v, input') <- p input, (w, input'') <- q input']

bind :: Parser a -> (a -> Parser b) -> Parser b
parser `bind` fn = 
  \input -> concat [fn v input' | (v, input') <- parser input]

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = item `bind` \x -> if pred x then success x else failure
  
-- specific character parser
char :: Char -> Parser Char
char c = satisfy (== c)

-- digit parser
digit :: Parser Char
digit = satisfy isDigit

-- lower-case parser
lower :: Parser Char
lower = satisfy isLower

-- upper-case parser
upper :: Parser Char
upper = satisfy isUpper


(&&&) :: Parser a -> Parser a -> Parser a
p &&& q = \input -> (p input ++ q input)
  
letter :: Parser Char
letter = lower &&& upper

alphanum :: Parser Char
alphanum = letter &&& digit

main :: IO ()
main = do
  -- print "item ->"
  -- print (item "")
  -- print (item "h")
  -- print (item "hello")
  -- print "failure ->"
  -- print (failure "hell")
  -- print "success ->"
  -- print (success 'd' "hello")
  -- print "sequence ->"
  -- print ((success 'd' ~> item) "hello")
  -- print ((failure ~> item) "hello")
  -- print ((item ~> success 'd') "hello")
  -- print "bind ->"
  -- print ((item `bind` success) "hello")
  -- print ((failure `bind` success) "hello")
  -- print "satisfy ->"
  -- print (satisfy (\ch -> ch == 'h') "hello")
  -- print (satisfy (\ch -> ch == 'd') "hello")
  -- print "char -> "
  -- print (char 'h' "hello")
  -- print (char 'd' "hello")
  -- print "digit ->"
  -- print (digit "54345")
  -- print (digit "hello")
  print "lower ->"
  print (lower "hello")
  print (lower "Hello")
  print "upper ->"
  print (upper "hello")
  print (upper "Hello")
  print "letter ->"
  print (letter "hello")
  print (letter "Hello")
  print "alphanum ->"
  print (alphanum "5ello")
  print (alphanum "Hello")
  print (alphanum "hello")
  
  
