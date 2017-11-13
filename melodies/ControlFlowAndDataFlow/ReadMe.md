# FizzBuzz

FizzBuzz is a mathematical game for children (of all ages), where you are required to count, 1, 2, ... but when you get to a number which is divisible by three, you should say "Fizz" instead of the number. Likewise, you should say "Buzz" for any number which is divisible by five. And of course, "FizzBuzz" for a number which is divisible by both three AND five.

We will attempt to write some code capable of playing FizzBuzz. First, without displaying the numbers - only the words:

Part 0
------
Write code that for a contiguous range of numbers prints out the following:

* 'Fizz' for numbers that are multiples of 3
* 'Buzz' for numbers that are multiples of 5
* 'FizzBuzz' for numbers that are multiples of 15

e.g. Running over a range from 1-20 should give the following output:

```
"" "" Fizz "" Buzz Fizz "" "" Fizz Buzz "" Fizz "" "" FizzBuzz "" "" Fizz "" Buzz
```

Part 1
------
The same as Part 1, except now we want to see the numbers displayed for numbers which are not divisible by either 3 or 5:

```shell
1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz 16 17 Fizz 19 Buzz
```

**BRAHMA** Lets look at implementing Fizz Buzz in Haskell.  For simplicity and gradually building the solution in 2 parts as put above.  

**BRAHMA** A naive implementation of FizzBuzz for a programmer coming from Imperative Paradigm to Functional Programming Paradigm would be to write a simple function - ```toFizzBuzz``` that produces either ```Fizz```, ```Buzz``` or ```FizzBuzz``` for a given Integer. 

```haskell
toFizzBuzz :: Int -> String
toFizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3 == 0 = "Fizz"
  | n `mod` 5 == 0 = "Buzz"
  | otherwise =  ""
```

**BRAHMA**  Instead of looping as one would in an imperative mode, I'd then apply a mapping function for all values until ```n```:

```haskell
fizzBuzz :: Int -> [String]
fizzBuzz n = map toFizzBuzz [1..n]

main :: IO ()
main = do  
  print $ fizzBuzz 20 -- ["","","Fizz","","Buzz","Fizz","","","Fizz","Buzz","","Fizz","","","FizzBuzz","","","Fizz","","Buzz"]
```

**BRAHMA** ...and for Part-1, I simply have to replace last ```else``` 

```haskell
toFizzBuzz :: Int -> String
toFizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3 == 0 = "Fizz"
  | n `mod` 5 == 0 = "Buzz"
  | otherwise = show n

fizzBuzz :: Int -> [String]
fizzBuzz n = map toFizzBuzz [1..n]

main :: IO ()
main = do  
  print $ fizzBuzz 20 -- ["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz","11","Fizz","13","14","FizzBuzz","16","17","Fizz","19","Buzz"]
```

**BRAHMA** So, this was a naive functional programming paradigm rendition of FizzBuzz.  Krishna, how does this look in an array-oriented language like APL?  

**KRISHNA** I can generate a 20-item array which repeats ('' '' 'Fizz') over and over again as follows:

```apl
    20⍴¯3↑'' 'fizz'
     fizz      fizz      fizz      fizz      fizz      fizz     
```

**KRISHNA** First a reminder: the syntax of APL uses the same rule as (f g x) in mathematics: The right argument of each function is the result of executing everything to the right. So the expression above is evaluated as:

```apl
    20⍴ (¯3↑ ('' 'fizz'))
```

**KRISHNA** Although APL *executes* from right to left, the correct way to read the above expression is "the 20 reshape of the negative 3 take of ('' 'Fizz'). The same way as one would say "f of g of x" for (f g x). If an expression gets too long to read it that way comfortably, one should probably break it up.

**KRISHNA** So... I start with a 2-element array containing an empty vector and 'Fizz'. The operation ```¯3↑``` creates an array of size 3 on the left with empty vectors (a positive ```3↑``` would add trailing items). The result is passed to ```20⍴``` is applied to the result, repeating the 3-element vector until it fills an array of size 20. Now, I can write and immediately apply an anonymous function to combine sequences of length 3 and 5 by inserting a ```,¨```(catenate each) between the lists:

```apl
    {(⍵⍴¯3↑'' 'fizz') ,¨ (⍵⍴¯5↑'' 'buzz')} 20
     fizz    buzz  fizz      fizz  buzz    fizz      fizzbuzz      fizz    buzz 
```
**KRISHNA** The curly braces ```{}``` enclose a function body, within which ```⍵``` (omega is the last letter in the greek alphabet) refers to the right argument. If there was a left argument, it would be ```⍺``` (alpha). Due to the order of execution, the rightmost set of parentheses are redundant, but have been added for symmetry.

**KRISHNA** Moving on to part 1, we are now supposed to include the underlying number. At this point, it starts to feel more natural to an APL user to take as input an array of integers (which could be a contiguous range, but could also be ANY set of integers), and replace some of them with text. To illustrate, I will start by defining an array which I will call ```input```:

```apl
    ⎕←input←⍳20 ⍝ iota (Index generator) produces the first 20 integers
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 
```

**KRISHNA** The ```⎕←``` at the left of the expression instructs the interpreter to also assign the result of the expression to the terminal - in other words output it. Without this, an assignment would produce no output.

**KRISHNA** Next, I'll will use the residue function ```|```, which computes its right argument modulus the left argument. Residue is what we call a "scalar function", which means that it applies to all scalars within the argument arrays. If I don't assign the result to a variable, the default is to print the result:

```apl
    3|input ⍝ inner product with residue
1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2
```

**KRISHNA** Now, I will use the ```outer product``` to do the computation for both 3 and 5 at the same time. ```∘.f``` combines each item of the left argument with each item of the right argument, in this case producing a (2×20) array. One row for each element of the left argument, one column for each in the right:

```apl
    3 5 ∘.| input  ⍝ outer product with residue
1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2
1 2 3 4 0 1 2 3 4 0 1 2 3 4 0 1 2 3 4 0
```

**KRISHNA** If course, what I'm really interested is in the positions where the input is a multiple of 3 or 5, which I can compute by comparing the above to ```0```. Note that I've reversed the order of 3 and 5 below, for reasons which I will explain in a moment:

```apl
    input←⍳20 ⍝ Remember that input is the integers 1-20
    0 = 5 3 ∘.| input
0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1
0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0
```

**KRISHNA** APL was invented as a rationalised mathematical notation, and has primitives designed to work on polynomials. One of these operations is the base value function ```⊥```. It takes a vector of coefficients and computes the polynomial that the vector represents using the left argument as the base. For example, ```x⊥3 ¯1 5``` computes (3x<sup>2</sup> - x<sup>1</sup> + 5x<sup>0</sup>), or more simply (3x<sup>2</sup> - x + 5). Applied to a matrix, base value interprets each *column* as a polynomial. This allows me to interpret each number above as a number base two (most significant digit = twos in the top row, least significant = ones in the bottom):

```apl
    ⎕←case← 2⊥ 0 = 5 3 ∘.| input
0 0 1 0 2 1 0 0 1 2 0 1 0 0 3 0 0 1 0 2
```

**KRISHNA** In the above vector, a 1 means the number was divisible by 3 (Fizz), a 2 means divisible by 5 (Buzz), and 3 means both 3 and 5 (FizzBuzz). I reversed the order of 3 and 5 to put the 5 in the top row, which is the most significant digit of the polynomial - in order for 1 to be 3 and 2 to be 5. This doesn't really matter, of course - just feels a bit more natural.

**KRISHNA** I'm nearly done; all I need to do now is to replace elements of input corresponding to non-zero elements of ```case```, with a text selected by the same element. I can find the indices of the non-zero elements using ```where``` (⍸):

```apl
    ⍸case≠0
3 5 6 9 10 12 15 18 20
```
**Dhaval asks** how about ```⎕←indices←⍸case≠0```?

**KRISHNA** The above gives me the indices of items to be replaced. I can generate the list of replacement values by indexing an array of 3 strings by the array of non-zero cases as follows. The "without" function ```~``` returns the left argument excluding any elements found in the right:

```apl
      ⎕←texts←('Fizz' 'Buzz' 'FizzBuzz')[case~0]
 Fizz  Buzz  Fizz  Fizz  Buzz  Fizz  FizzBuzz  Fizz  Buzz 
```

**KRISHNA** I can compute the final result using the ```@``` operator to merge these values into my input at the relevant positions. I would read the expression below as "texts merged with input at positions where case is not zero":

**Dhaval asks** how about ```(texts @ indices) input```?

```apl
    (texts @ (⍸case≠0)) input
1 2  Fizz  4  Buzz  Fizz  7 8  Fizz  Buzz  11  Fizz  13 14  FizzBuzz  16 17  Fizz  19  Buzz 
```

**KRISHNA** Having experimented a bit interactively, I can now define a function which brings it all together. The code within curly braces below is a function, which I have named FizzBuzz. Within the function, ```⍵``` refers to the right argument. In this function, I use ```reverse first``` (⊖) to reverse the order of the primes - rather than having the constant in the wrong order, as I did while experimenting:

```apl
    FizzBuzz←{                                                                            
      primes←3 5                                                                        
      text←'Fizz' 'Buzz' 'FizzBuzz'                                                     
      case←2⊥⊖0=primes∘.|⍵ ⍝ 0=nothing, 1=Fizz, 2=Buzz, 3=FizzBuzz                      
      (messages[case~0]@(⍳case≠0)) ⍵ ⍝ merge text where case is non-zero                                            
    }
```

**KRISHNA** And now I can compute the result on any array of integers - not just the integers from 1 to n. To illustrate, here is the result of FizzBuss on the numbers from 21-30: 
      
```apl
      FizzBuzz 20+⍳10
 Fizz  22 23  Fizz  Buzz  26  Fizz  28 29  FizzBuzz 
```

**KRISHNA** Note that, since many APL primitives map to arrays implicitly, this solution has no loops or conditional statements. Another important fact here is that the data structure drives the application of functions, and there is no control flow in the code.

**KRISHNA** Do you want to have a go at producing a data-driven solution?

**BRAHMA** Yes, let me give it a go in Haskell.

```haskell
fizzBuzz :: Int -> [String]
fizzBuzz n = ???

main :: IO ()
main = do
  print $ fizzBuzz 20
```

**BRAHMA** Haskell library has a cycle function that produces a stream of data.  So I'll use that to produce 3s and 5s.  Then use ```zipWith``` to concatenate one element from each list. 

```haskell
fizzBuzz :: Int -> [String]
fizzBuzz n = take n result
    where
        threes = cycle ["", "", "Fizz"]
        fives = cycle ["", "", "", "", "Buzz"]
        result = zipWith (++) threes fives
        
main :: IO ()
main = do
  print $ fizzBuzz 20 -- ["","","Fizz","","Buzz","Fizz","","","Fizz","Buzz","","Fizz","","","FizzBuzz","","","Fizz","","Buzz"]
```

**BRAHMA** Having completed that, I can implement Part-1 now, where I have to print indices instead of empty strings.  So, I introduce ```indices``` as a list of strings by using the ```map show``` to convert list of integers to strings.  I change ```zipWith``` to ```zipWith3``` accommodate ```indices``` as the third argument.

```haskell
fizzBuzz :: Int -> [String]
fizzBuzz n = take n result
    where
        threes = cycle ["", "", "Fizz"]
        fives = cycle ["", "", "", "", "Buzz"]
        indices = map show [1..n]
        result = zipWith3 ??? threes fives indices
        
main :: IO ()
main = do
  print $ fizzBuzz 20
```

**BRAHMA**  I now need to re-write the lambda inside ```zipWith3```

```haskell
fizzBuzz :: Int -> [String]
fizzBuzz n = take n result
    where
        threes = cycle ["", "", "Fizz"]
        fives = cycle ["", "", "", "", "Buzz"]
        indices = map show [1..n]
        select = \t f i -> if (t == "" && f == "") then i else t ++ f
        result = zipWith3 select threes fives indices
        
main :: IO ()
main = do
  print $ fizzBuzz 20  -- ["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz","11","Fizz","13","14","FizzBuzz","16","17","Fizz","19","Buzz"]
```

**BRAHMA**  So this is how I can be data-driven in Haskell.

**KRISHNA**  Cool. My only worry is that are passing the lambda and applying it for every element in the lists... processing one at a time, and working on lazy cycles of strings, which seems a bit heavy to me compared to working with arrays without any loops.

**BRAHMA**  I want to then ask you this - is the APL interpreter not looping inside the primitive operations?

**KRISHNA**  Yes, of course it is... but it is looping in highly optimised C, on dense arrays of booleans and small integers. The fact that we are performing the same computation on evey item of an array provides opportunities to vectorise the operations, or parallelise them on a GPU. 

**BRAHMA**  I see. Of course, that means you need to fully materialise the arrays, which could consume a lot of memory - right? In our Haskell solution, everything is lazy by default and so is list data-structure, so entire list is never materialized, only the needed element is brought in one at a time in memory and GCed after use.  Though it appears that we are process element-by-element, one cannot tell, theoretically, whether the ```zipWith``` function is splitting into data chunks or whether data is CPU bound or GPU bound.  The how-part is abstracted away - the code is declarative.  But, yes the lambda is applied to every element during zipping is a fact.

**BRAHMA**  It is interesting how the features of a language suggest solutions. Now that I have seen the APL solution, I can write something similar in Haskell - although that solution would have not occurred to me before. I can't yet see how to get rid of lambda, but I could do something like this:

```haskell
import Data.Digits (unDigits)

fizzBuzz :: Int -> [String]
fizzBuzz n = take n result
    where
        threes = cycle [0, 0, 1]
        fives  = cycle [0, 0, 0, 0, 1]
        indices = [1..]
        fizzBuzz = ["", "Fizz", "Buzz", "FizzBuzz"]
        base2 = unDigits 2
        select = \f t i -> if(base2 [f,t] == 0) then show i else fizzBuzz !! (base2 [f,t])
        result = zipWith3 select fives threes indices
    
main :: IO ()
main = do
  print $ fizzBuzz 20  -- ["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz","11","Fizz","13","14","FizzBuzz","16","17","Fizz","19","Buzz"]
```

**KRISHNA**  An interesting question would be whether that allows the Haskell compiler to produce a better solution. Of course, compilers and interpreters are often optimised for idiomatic use of that language, so you need to be careful: something which looks more efficient might not be, if it takes the compiler by surprise.

**KRISHNA**  Likewise, I think I can simplify the APL solution by using a lambda. I will use an infix function, in which ```⍺``` refers to the left argument and ```⍵``` the right (as before). In APL, user-defined lambdas have the same syntax as primitive functions, they are either infix or prefix. A prefix function which returns a 2-element vector showing whether the a single-element right argument was divisible by 3 or 5 could be written:

```apl
      {0=3 5|⍵} 10
0 1
``` 
**KRISHNA** We can apply this lambda to the items of a vector using the each operator ```¨```. _Note that in APL, we use the term operator to refer to higher order functions, which take functions as operands_. In this case, each takes our lambda on the left and creates a derived function which can be applied to arrays of any rank, returning an array of the same shape as the argument:

```apl
      {0=3 5|⍵}¨1 3 5 7 15
 0 0  1 0  0 1  0 0  1 1 
```

**KRISHNA** Adding a NOR reduction to tell us whether the number was not divisible, in other words we should keep it, rather than replace it with a string:  

```apl

      {⍱/0=3 5|⍵}¨1 3 5 7 15
1 0 0 1 0
```
**KRISHNA** The final step is to use the above result as a guard. A guard is an logical expression followed by a colon and an expression to evaluate if the guard expression returns true - in this case just ⍵, which means we return the right argument unchanged. We add a statement separator ```⋄``` (diamond), and an expression which will be evaluated if the guard does not fire:

```apl
      {⍱/m←0=3 5|⍵:⍵ ⋄ ∊m/'Fizz' 'Buzz'}¨⍳20
1 2  Fizz  4  Buzz  Fizz  7 8  Fizz  Buzz  11  Fizz  13 14  FizzBuzz  16 17  Fizz  19  Buzz 
```

**KRISHNA** Above, I save the result of the residue calculations in a two-element boolean array named ```m``` (for mask). In the second expression (following the statement separator ```⋄```), I use ```m``` to select corresponding elements of a vector containing the two strings Fizz and Buzz, and finally use the enlist function ```∊``` to remove the structure of the result and return a simple character array, which will contain Fizz if m is (1 0), Buzz if m is (0 1), or FizzBuzz if m is (1 1).

**KRISHNA** In many ways, applying a lambda with each is simpler than taking the time to write an array-oriented solution. An APL programmer might well start here in order to "brute force" a solution in a hurry. If the code becomes a performance bottleneck, the array based solution will be much more efficient, it terms of both memory and CPU time.

**KRISHNA** Lets reflect on this...

Reflections
-----------

**KRISHNA** Most programming languages emphasize the use of control flow for managing programming logic...like the initial attempt in Haskell, the logic is present in the ```toFizzBuzz``` function.   

**KRISHNA** Logic in APL is often most efficiently expressed in a data representation.  It is this representation that is processed using array operations, achieving the end result.  

**BRAHMA** The second attempt in Haskell focussed on data, so rather than the source code that embeds the logic in the control flow of the program.  In APL the logic is embedded in the data flow.

**BRAHMA** So, essentially this an eye-opening contrast - Control Flow or Data-Flow? It is important to take the data-first approach, because it is much easier to deal with changing data than to change program-logic.  Humans are better at visualizing data, rather than reasoning about control-flow.  This applies every where - whether it is functional programming paradigm or array-oriented paradigm or Object-Oriented Paradigm.  Lets move to the next melody in our jugalbandi.
