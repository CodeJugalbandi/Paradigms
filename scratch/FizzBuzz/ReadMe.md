#FizzBuzz


Part 0
------
Write code that for a contiguous range of numbers prints out the following:

* 'Fizz' for numbers that are multiples of 3
* 'Buzz' for numbers that are multiples of 5
* 'FizzBuzz' for numbers that are multiples of 15

e.g. Running over a range from 1-20 should give the following output

```
"" "" Fizz "" Buzz Fizz "" "" Fizz Buzz "" Fizz "" "" FizzBuzz "" "" Fizz "" Buzz
```

Part 1
------
Write some code that for a contiguous range of numbers prints out the following:

* the number
* 'Fizz' for numbers that are multiples of 3
* 'Buzz' for numbers that are multiples of 5
* 'FizzBuzz' for numbers that are multiples of 15


e.g. Running over a range from 1-20 should give the following output

```shell
1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz 16 17 Fizz 19 Buzz
```

Code Jugalbandi in Functional Programming & Array-Oriented Paradigms
----

**BRAHMA** Lets look at implementing Fizz Buzz in Haskell.  For simplicity and gradually building the solution in 2 parts as put above:

```haskell
fizzBuzz :: Int -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3 == 0 = "Fizz"
  | n `mod` 5 == 0 = "Buzz"
  | otherwise =  ""

main :: IO ()
main = do  
  print $ map fizzBuzz [1..20] -- ["","","Fizz","","Buzz","Fizz","","","Fizz","Buzz","","Fizz","","","FizzBuzz","","","Fizz","","Buzz"]
```

**BRAHMA** ...and for Part-1, I simply have to replace last ```else``` 

```haskell
fizzBuzz :: Int -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3 == 0 = "Fizz"
  | n `mod` 5 == 0 = "Buzz"
  | otherwise = show n

main :: IO ()
main = do  
  print $ map fizzBuzz [1..20] -- ["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz","11","Fizz","13","14","FizzBuzz","16","17","Fizz","19","Buzz"]
```

**BRAHMA** So, this was a vanilla implementation of FizzBuzz.  Krishna, how does this look in an array-oriented language like APL?  

**KRISHNA** To an APL user, the first idea that will spring to mind is to start with an array of the integers 1-20, and replace some of them with text. I'll start in an array which I will call ```input```:

```apl
    ⎕←input←⍳20 ⍝ iota (Index generator) produces the first 20 integers
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 
```

**KRISHNA** The ```⎕←``` at the left of the expression instructs the interpreter to also assign the result of the expression to the terminal - in other words output it.

**KRISHNA** Next, I'll will use the residue function ```|```, which computes the right argument modulus the left argument. In APL, the "map" is implicit and primitives apply to all items of the argument arrays. If I don't assign the result to a variable, the default is to print the result:

```apl
    3|input ⍝ inner product with residue
1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2
```

**KRISHNA** Now, I will use the ```outer product``` to do the computation for both 3 and 5 at the same time. ```∘.f``` combines each item of the left argument with each item of the right argument, in this case producing a (2×20) array:

```apl
    3 5 ∘.| input  ⍝ outer product with residue
1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2
1 2 3 4 0 1 2 3 4 0 1 2 3 4 0 1 2 3 4 0
```

**KRISHNA** If course, what I'm really interested is in the positions where the input is a multiple of 3 or 5, which I can compute by comparing the above to ```0```. Note that I've reversed the order of 3 and 5 below, for reasons which will soon become apparent.

```apl
    input←⍳20 ⍝ Remember that input is the integers 1-20
    0 = 5 3 ∘.| input
0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1
0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0
```

**KRISHNA** APL was invented as a rationalised mathematical notation, and has primitives designed to work on polynomials. One of these operations is ```⊥```, also known as ```decode```. It takes a vector of coefficients and computes the polynomial that the vector represents using the left arguent as the base. For example, ```x⊥3 ¯1 5``` computes (3x^2 - x + 5). Applied to a matrix, base value interprets each column as a polynomial. This allows me to interpret each number above as a number base two (most significant digit = twos in the top row, least significant = ones in the bottom):

```apl
    ⎕←case← 2⊥ 0 = 5 3 ∘.| input
0 0 1 0 2 1 0 0 1 2 0 1 0 0 3 0 0 1 0 2
```

**KRISHNA** In the above vector, a 1 means the number was divisible by 3 (Fizz), a 2 means divisible by 5 (Buzz), and 3 means both 3 and 5 (FizzBuzz). We had to reverse the order of 3 and 5 to put the 5 in the top row, which is the most significant digit of the polynomial.

**KRISHNA** I'm nearly done; all I need to do now is to replace elements of input corresponding to non-zero elements of ```case```, with a text selected by the same element. I can find the indices of the non-zero elements using ```where``` (⍸):

```apl
    ⍸0≠case
3 5 6 9 10 12 15 18 20
```

**KRISHNA** The above gives me the indices of items to be replaced. I can generate the list of replacement values by indexing an array of 3 strings by the array of non-zero cases as follows:

```apl
      ⎕←texts←('Fizz' 'Buzz' 'FizzBuzz')[case~0]
 Fizz  Buzz  Fizz  Fizz  Buzz  Fizz  FizzBuzz  Fizz  Buzz 
```

**KRISHNA** I can compute the final result using the ```@``` operator to merge these values into my input at the relevant positions:

```apl
    (texts@(⍸0≠case)) input
1 2  Fizz  4  Buzz  Fizz  7 8  Fizz  Buzz  11  Fizz  13 14  FizzBuzz  16 17  Fizz  19  Buzz 
```

**KRISHNA** Having experimented a bit interactively, I can now define a function which brings it all together. The code within curly braces below is a function, and ```⍵``` refers to the right argument. For clarity, I have decided to have the primes in ascending order, and use a ```reverse first``` (⊖) to reverse the order of the rows before applying decode:

```apl
    FizzBuzz←{                                                                            
      primes←3 5                                                                        
      text←'Fizz' 'Buzz' 'FizzBuzz'                                                     
      case←2⊥⊖0=primes∘.|⍵ ⍝ 0=nothing, 1=Fizz, 2=Buzz, 3=FizzBuzz                      
      (messages[case~0]@(⍳0≠case)) ⍵ ⍝ merge text where case is non-zero                                            
    }
```

**KRISHNA** And now I can compute the result on any array of integers. Below I'll do it on the numbers from 21-30: 
      
```apl
      FizzBuzz 20+⍳10
 Fizz  22 23  Fizz  Buzz  26  Fizz  28 29  FizzBuzz 
```

**KRISHNA** Note that, since APL primitives map to arrays implicitly, this solution has no loops or conditional statements. The data structure drives the application of functions, and there is no control flow in the code.

**KRISHNA** Do you want to have a go at producing a data-driven solution?

**BRAHMA** Yes, let me give it a go in Haskell.  We need a ```main``` and I need to call ```fizzBuzz``` for 20 numbers.  So fizzBuzz is a function that consumes an ```Int``` and returns a list of ```String```

```haskell
fizzBuzz :: Int -> [String]
fizzBuzz n = ???

main :: IO ()
main = do
  print $ fizzBuzz 20
```

**BRAHMA** Haskell library has a cycle function that produces a stream of data.  So i'll use that to produce 3s and 5s.  Then use ```zipWith``` to concatenate one element from each list. 

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

**KRISHNA**  Of course it is... but it is looping in highly optimised C, on dense arrays of booleans and small integers. The fact that we are performing the same computation on evey item of an array provides opportunities to vectorise the operations, or parallelise them on a GPU. 

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

**KRISHNA**  An interesting question would be whether that allows the Haskell compiler to produce a better solution. Of course, compilers and interpreters are ofter optimised for idiomatic use of that language, so you need to be careful: something which looks more efficient might not be, if it takes the compiler by surprise.

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

**KRISHNA** We stored the temporary result of the two residue calculations in a variable called m (for mask), use this to select corresponding elements of a vector containing the two strings Fizz and Buzz, and finally use the enlist function ```∊``` to remove the structure of the result and return a simple character array, which will contain Fizz if m is (1 0), Buzz if m is (0 1), or FizzBuzz if m is (1 1).

**KRISHNA** In many ways, the lambda-based expression is arguably simpler than the array-oriented one - and an APL programmer might well start here in order to "brute force" a solution in a hurry. If the code becomes a performance bottleneck, the array based solution will be much more efficient, it terms of both memory and CPU time.

**KRISHNA** Lets reflect on this...

Reflections
-----------

**KRISHNA** Most programming languages emphasize the use of control flow for managing programming logic...like the initial attempt in Scala, the logic is present in the ```toFizzBuzz``` function.   

**KRISHNA** Logic in APL is often most efficiently expressed in a data representation.  It is this representation that is processed using array operations, achieving the end result.  

**BRAHMA** So rather than rendering the source code that embeds the logic in the control flow of the program, in APL the logic is embedded in the data flow.

**BRAHMA** So, essentially this an eye-opening contrast - Control Flow or Data-Flow? Lets move to the next melody in our jugalbandi.

