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

Part 2
------
Enhance FizzBuzz solution to perform the following:

* If the number contains a three you must output the text 'Lucky'. This overrides any existing behaviour

e.g. Running over a range from 1-20 should give the following output

```
1 2 Lucky 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz Lucky 14 FizzBuzz 16 17 Fizz 19 Buzz
```

Code Jugalbandi in Functional Programming & Array-Oriented Paradigms
----

**BRAHMA** Lets look at implementing Fizz Buzz in Scala.  For simplicity and gradually building the solution in 2 parts as put above:

```scala
def toFizzBuzz(n: Int) = {
    if (n % 15 == 0) "FizzBuzz"
    else if (n % 3 == 0) "Fizz"
    else if (n % 5 == 0) "Buzz"
    else ""
}                                             

val fizzBuzzed = (1 to 20) map toFizzBuzz
println(fizzBuzzed)
```

**BRAHMA** ...and for Part-1, I simply have to replace last ```else``` 

```scala
def toFizzBuzz(n: Int) = {
    if (n % 15 == 0) "FizzBuzz"
    else if (n % 3 == 0) "Fizz"
    else if (n % 5 == 0) "Buzz"
    else n
}                                             

val fizzBuzzed = (1 to 20) map toFizzBuzz
println(fizzBuzzed)
```

**BRAHMA** So, this was a vanilla implementation of FizzBuzz.  Krishna, how does this look in an array-oriented language like APL?  

**KRISHNA** Well, array-oriented languages use array as the *only* data structure and so instead of thinking in terms of flow, we think in terms of data.  Let me show you how this would look like in APL.  I will straight away implement Part-1 of the problem:

```apl
    ⍳20 ⍝ iota (Index generator) produces 20 numbers
⍝ 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 
```

**KRISHNA** And, I'll assign it to ```input```

```apl
    input ← ⍳20 ⍝ iota (Index generator) produces 20 numbers
```

**KRISHNA** Next, I'll prepare to mark every third element in the vector using the residue function ```|```, which you call in other languages as ```mod```.

```apl
    input ← ⍳20 
    3|input ⍝ inner product with residue
⍝ 1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2
```

**KRISHNA** Similarly for marking every 5th element, I can compute

```apl
    input ← ⍳20
    5|input ⍝ 5 residue = remainder after dividing by 5
⍝ 1 2 3 4 0 1 2 3 4 0 1 2 3 4 0 1 2 3 4 0
```

**KRISHNA** I can combine both the ebove operations in a single step by using the outer product with residue, between a vector containing 3 and 5 on the left, and our input on the right:

```apl
    input ← ⍳20
    5 3 ∘.| input  ⍝ outer product with residue
⍝ 1 2 3 4 0 1 2 3 4 0 1 2 3 4 0 1 2 3 4 0
⍝ 1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2
```

**KRISHNA** So, now that I have obtained a result array which is 2x20, i.e. 2 rows and 20 columns.  You can check the rank using the operator rho - ```⍴```

```apl
    input ← ⍳20 
    ⍴5 3 ∘.|input  ⍝ ⍴ - shape or reshape operator.
⍝ 2 20
```

**KRISHNA** Now that I have these vectors, what I'm really interested is in the positions where the input is a multiple of 3 or 5, so I compare the result of residue to  ```0``` and obtain a vector of binary values. Note that, in APL an array containing only 0 or 1 is stored using only 1 bit per element, which makes code fast and memory efficient. The ```⎕←``` below assigns the result to the terminal, symbolised by ```⎕```, so the value is displayed.

```apl
    input ← ⍳20 
    ⎕←fivesAndThrees ← 0  =5 3 ∘.| input
0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1
0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0
```

**KRISHNA** Now that I have these vectors denoting positions of Fizz's, Buzz's and FizzBuzz's, I need to merge some corresponding text at appropriate positions in the input array.  In APL, if I have a vector of strings and I need a particular element at that index, I can select from it using indices - wit the difference that in APL, the indices can also be an array, thus making multiple selections. 

```apl
      ('Fizz' 'Buzz' 'FizzBuzz')[1]
⍝ Fizz 
      ('Fizz' 'Buzz' 'FizzBuzz')[2]
⍝ Buzz 
      ('Fizz' 'Buzz' 'FizzBuzz')[1 2 3]
⍝ Fizz  Buzz  FizzBuzz 
```

**KRISHNA** If I look at the array 2x20, I need to generate these indices from that vector, then index into the FizzBuzz vector and get the required elements out.  Lets see how pull this together.

```apl
⍝ Row 1 => 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1
⍝ Row 2 => 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0
```

**KRISHNA** Close inspection of the data gives a interesting realization.  If I am able to merge columns and produce a 20-element vector with positions denoting the presence of either Fizz or Buzz or FizzBuzz, I would be more than half-way there.  We can view each column as a 2-digit binary number with values between 0 and 3. For examples the first column is 0 0, giving a value of 0 when the base is 2, the third column has 0 1, giving 1, the fifth column has 1 0, which means 2 and the 15th has 1 1, which means 3.  APL has a function called decode, denoted with the Up Tack - ```⊥```, which takes a base on the left and "decodes" vectors, or in this case the columns of a matrix, using the supplied base:

```apl
    input ← ⍳20 
    fivesAndThrees ← 0=5 3 ∘.|input
    ⎕←select ← 2⊥fivesAndThrees
0 0 1 0 2 1 0 0 1 2 0 1 0 0 3 0 0 1 0 2
```

**KRISHNA** Before I use these as indices, I need to weed out 0s because we need to deal with the fact that array indices in APL begin with 1 and in the above output we have 0s as well. In functional programming, you will probably do a filter operation to weed 0s out and then use the indices to index into the array.  I do all of that in one shot with the iota underbar ```⍸``` operator in APL.  This operator in its monadic form takes a binary vector and returns the indices where 1s are found:

```apl
    ⍸1 0 1 1
⍝ 1 3 4
```

**KRISHNA** ```select``` in our case contains non-binary values, so we compare using the ```≠``` operator.

```apl
    input ← ⍳20 
    fivesAndThrees ← 0=5 3 ∘.|input
    select ← 2⊥fivesAndThrees
    0≠select
⍝ 0 0 1 0 1 1 0 0 1 1 0 1 0 0 1 0 0 1 0 1
```
**KRISHNA** Now applying the ```⍸``` operator, we get indices at which 'Fizz', 'Buzz' or 'FizzBuzz' will be present. I will name this vector ```indices```

```apl
    input ← ⍳20 
    fivesAndThrees ← 0=5 3 ∘.|input
    select ← 2⊥fivesAndThrees
    indices ← ⍸0≠select
⍝ 3 5 6 9 10 12 15 18 20
```
**KRISHNA** With indices in hand, I can now look up the coresponding items of ```select``` and get the indices to pick up messages.

```apl
⍝ output of select
⍝ 0 0 1 0 2 1 0 0 1 2 0 1 0 0 3 0 0 1 0 2
⍝ output of Iota underbar
⍝ 3 5 6 9 10 12 15 18 20

    select[indices]
⍝ 1 2 1 1 2 1 3 1 2

    'Fizz' 'Buzz' 'FizzBuzz'[select[indices]]
⍝ Fizz  Buzz  Fizz  Fizz  Buzz  Fizz  FizzBuzz  Fizz  Buzz 
```

**KRISHNA** So, the whole thing now looks like this:

```apl
    input ← ⍳20 
    fivesAndThrees ← 0=5 3 ∘.|input
    select ← 2⊥fivesAndThrees
    indices ← ⍸0≠select
    messages ← 'Fizz' 'Buzz' 'FizzBuzz'
    messages[select[indices]]
    
⍝ Fizz  Buzz  Fizz  Fizz  Buzz  Fizz  FizzBuzz  Fizz  Buzz   
```

**KRISHNA** Finally, I simple need to use this to update the input at index positions:

```apl
    input ← ⍳20 
    fivesAndThrees ← 0=5 3 ∘.|input
    select ← 2⊥fivesAndThrees
    indices ← ⍸0≠select
    messages ← 'Fizz' 'Buzz' 'FizzBuzz'
    input[indices] ← messages[select[indices]]
    
    input
    
⍝ 1 2  Fizz  4  Buzz  Fizz  7 8  Fizz  Buzz  11  Fizz  13 14  FizzBuzz  16 17  Fizz  19  Buzz 
```
**KRISHNA** The important thing to realize here is that I am playing with data and not flow.  There are no loops, simply data manipulations that go along.  Do you think you can do data-driven?

**BRAHMA** Yes, let me give it a go in Haskell.  So we need a ```main``` and I need to call ```fizzBuzz``` for 20 numbers.  So fizzBuzz is a function that consumes an ```Int``` and returns a list of ```String```

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
  print $ fizzBuzz 20
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
  print $ fizzBuzz 20
```

**BRAHMA**  So this is how I can be data-driven in Haskell.

**KRISHNA** But you still are passing the lambda and applying it for every element in the lists...processing one at a time.  With array-oriented languages, you simply don't think of loops.  It does not mean I cannot use lambdas in APL.  I could as well write the same using a lambda, but its not data-driven in essence.  Also, I am not working with strings, I am working with numbers and booleans, which is far more performant than strings, I'd say.

**BRAHMA**  I want to then ask you this - are you not looping behind the functions in APL?

**KRISHNA**  Yes, we are, but its in our discretion parallelize and put each computation on a GPU or CPU for instance.  I can then improve the performance and reduce memory consumed greatly. 

**BRAHMA** Well, as far as Haskell is concerned, it is lazy by default and so is list data-structure, so entire list is never materialized, only the needed element is brought in one at a time in memory and GCed after use.  Though it appears that we are process element-by-element, one cannot tell, theoretically, whether the ```zipWith``` function is splitting into data chunks or whether data is CPU bound or GPU bound.  The how-part is abstracted away - the code is declarative.  But, yes the lambda is applied to every element during zipping is a fact.

**BRAHMA** I must say one thing though, I can see how you evolved the algorithm, which I think would be difficult to think in the first go itself.  Let me use Haskell to re-write this in the APL form, but I think I still won't be able to get rid of lambda.

```haskell
import Data.Digits (unDigits)

fizzBuzz :: Int -> [String]
fizzBuzz n = take n result
    where
        threes = cycle [0, 0, 1]
        fives  = cycle [0, 0, 0, 0, 1]
        fizzBuzz = ["", "Fizz", "Buzz", "FizzBuzz"]
        base2 = unDigits 2
        select = \f t i -> if(base2 [f,t] == 0) then show i else fizzBuzz !! (base2 [f,t])
        result = zipWith3 select fives threes [1..]
    
main :: IO ()
main = do
  print $ fizzBuzz 20
```

**KRISHNA** Let me show you how this can be done using anonymous function in APL. Functions in APL are written using curly braces, within which ```⍺``` refers to the left argument and ```⍵``` the right. User-defined lambdas have the same syntax as primitive functions, they are either infix or prefix. A prefix function which returns a 2-element vector showing whether the a single-element right argument was divisible by 3 or 5 could be written:

```apl
      {0=3 5|⍵} 10
⍝ 0 1
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

**KRISHNA** In fact, the lambda-based expression is arguably simpler than the array-oriented one. However, the array based solution will be much more efficient, it terms of both memory and CPU time.

**KRISHNA** Lets reflect on this...

Reflections
-----------

**KRISHNA** Most programming languages emphasize the use of control flow for managing programming logic...like the initial attempt in Scala, the logic is present in the ```toFizzBuzz``` function.   

**KRISHNA** Logic in APL is often most efficiently expressed in a data representation.  It is this representation that is processed using array operations, achieving the end result.  

**BRAHMA** So rather than rendering source structure that embeds the logic in the control flow of the program, in APL the logic is embedded in the data flow.

**BRAHMA** So, essentially this an eye-opening contrast - Control Flow or Data-Flow? Lets move to the next melody in our jugalbandi.

