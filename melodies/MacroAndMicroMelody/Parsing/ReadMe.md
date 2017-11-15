# Parsing Text Containing Recursive Structures

Let us consider the problem of parsing arrays.  Examples could be:

* An empty array - ```"[]"```, 
* An array with single letter - ```"[a]"``` or a single digit - ```"[1]"```,
* An heterogeneous array with a letter and a digit - ```"[a,1]"```,
* Nested Arrays - ```"[[]]", "[a,[]]", "[a,[1]]", "[अ,१,[२,ब]]"```

The BNF (Backus-Naur Form) or EBNF (Extended BNF) grammar for Arrays containing letters, digits and nested arrays can be described as:

```
array  ::= "[" [ values ] "]".
values ::= value {"," value}.
value  ::= array | letter | digit.
```

Notation      | Meaning
------------- | -------------
```|```       | Alternative
```[ ... ]``` | Optional
```{ ... }``` | Repeated Zero, or One or Many times

 
**_NOTE_**: For simplicity, we will not consider whitespaces to be parsed.
 
Code Jugalbandi in Functional Programming & Array-Oriented Paradigms
----

**BRAHMA** We have agreed to play three verses: first we will write a parser for an array containing a single digit or letter (or none). Then we will look at how we need to adapt our solutions in order to extend them, first to parse arrays with more than one element, and finally nested arrays, where each item of an array can be another array.

**BRAHMA** Lets play the first verse in APL. The terseness of APL and the ability to experiment interactively encourages an "extreme" approach to solving problems, so I choose to write a minimal solution to the zero-or-one element case:

```apl
    ArrayParser←{
       '[]'≢(⊣/,⊢/)⍵ : 'missing outer []' ⎕SIGNAL 11 
       1<≢inner←1↓¯1↓⍵ : 'max one item allowed' ⎕SIGNAL 11
       ⊃inner∊⎕D: ⎕D⍳inner
       inner
     }
```

**BRAHMA** Each of the first three lines is a ```guard```, containing two expressions separated by a colon.  The expression on the left is a logical test; if the test is true the second expression is executed and it's result becomes the result of the function. In this case, the first two lines are performing validation.

**BRAHMA** The first test ```'[]'≢(⊣/,⊢/)⍵``` determines whether the string "[]" is different from a string formed from the first and last element of the argument.  If this is the case, we throw an exception using ```⎕SIGNAL``` and the function terminates.  We use the standard APL event number 11 (DOMAIN ERROR) with a custom error message, provided as the left argument.

**BRAHMA** The second test expression forms a new string called ```inner``` by dropping the first and last character from the argument, and then checks the length; if the length of ```inner``` is greater than one, an exception is thrown.

**BRAHMA** The third guard expression tests whether the first (and only) element of inner is a member of ```⎕D```, a system constant containing the digits 0..9.  If it is, the expresion on the right returns the index of the character in this array, essentially casting the character to an integer.  If the character is not a digit, we fall through to the 4th line, which simply returns the string ```inner``` as the result.

**BRAHMA** Krishna, how would you play this tune in an OO or Functional paradigm?

**KRISHNA** Ok, let me show you how this can be approached in a functional  paradigm.  I'll use composability and create a solution which may require a bit more work to begin with, but will be easy to extend to the more complex cases.  It would actually allow us to more or less use EBNF to specify the syntax. 

**KRISHNA** I'll use Scala once again, to construct a framework of ```Parser```s and ```infix``` compositional forms that will allow me to write expressions which closely resemble EBNF and thus combine them to produce an executable ```Parser```.  We can define a ```Parser``` as a function that consumes a ```String``` and produces a structure of type ```T```, containing a more useful representation of the string.

```scala
type Parser[T] = String => T
```

**KRISHNA**  A useful technique is to define a number of parsers which each look for a specific pattern and - if they find it - "consume" the part of the input string corresponding to that pattern.  We can change the return type to a tuple and also return the unused part of the string.  Returning the unused string allows us to chain parsers, so that one parser can operate after another until all input is consumed.

```scala
type Parser[T] = String => (T, String)
```

**KRISHNA** Each parser may or may not find the pattern it is looking for.  We can represent this by making the return of the type ```T```optional:

```scala
type Parser[T] = String => Option[(T, String)]
```

**KRISHNA** With this type in hand, we can now start implementing parsers.  I'll start with 3 very simple parsers which will serve as building blocks for complex parsers.

**KRISHNA** I'll define a ```success``` parser i.e., a parser that always succeeds.  Whatever value we feed to it, it returns the same value, without consuming any input.

```scala
def success[T](t: T): Parser[T] = 
  (in: String) => Some((t, in))
  
println(success(1)("world"))       // Some((1, "world"))
println(success("hello")("world")) // Some(("hello", "world"))
```

**KRISHNA** We now define a dual of the above parser, a parser that always fails - a ```failure``` parser.

```scala
def failure = (in: String) => None

println(failure("hello")) // None
println(failure(""))      // None
```

**KRISHNA** These parsers will be very useful to us when we want to return success or failure parsers after input validation. 

**KRISHNA** The third parser we will implement is one that consumes a single character from the string and returns that character along with the unused part of the string:

```scala
def anychar = (in: String) => 
  if (in.isEmpty) None else Some((in.head, in.tail))

println(anychar("hello")) // Some(h,ello)
println(anychar(""))      // None
```

**KRISHNA** A functional language allows us to start with simple parsers to find single digits or letters, and use function combinators to produce "compound" parsers which search for sequences of patterns (a sequence of digits gives a number) - or look for alternative patterns (an array item can be *either* a number *or* a string).

**KRISHNA** In order to be able to declare that we are looking for one pattern immediately followed by another, we can define a ```sequence``` combinator, that consumes two parsers and produces another parser which looks for that sequence of patterns.  The resulting parser feeds the initial input to the first parser, and the unused output from the first parser as input to the second parser.  The sequence combinator thus applies the two parsers one after the other and returns the results of the two parsers as a pair along with the residual output from the second parser:

```scala
def sequence[T, U](p: Parser[T], q: Parser[U]): Parser[(T,U)] = 
  (in: String) => for {
      (x, out1) <- p(in)
      (y, out) <- q(out1)
  } yield ((x,y), out)

println(seq(anychar, success("hello"))("world")) // Some(((w,hello),orld))
println(seq(success("hello"), anychar)("world")) // Some(((hello,w),orld))
println(seq(anychar, failure)("world")) // None
```

**KRISHNA** In order to get closer to writing code which looks like EBNF, we need our combinators to be infix operators.  I will take advantage of Scala's ```implicit class``` mechanism and rename ```sequence``` as an infix operator symbol ```~```.   Scala requires that an implicit class must have a constructor that takes in a parameter on which the operator can be invoked (in other words a receiver).  So the first parameter ```p: Parser[T]``` goes in as constructor parameter and the second remains with the method itself:

```scala
implicit class ParserExtensions[T](p: Parser[T]) {
  def ~ [U](q: Parser[U]): Parser[(T,U)] = 
    (in: String) => for {
      (x, out1) <- p(in)
      (y, out) <- q(out1)
    } yield ((x,y), out)
}
```

**KRISHNA** For convenience, I'll also define a ```parse``` function, like this, so we can invoke any parser or combination of parsers on a string:

```scala
def parse[T](in: String, p: Parser[T]) = p(in)

println(parse("world", anychar ~ anychar)) // Some(((w,o),rld))
println(parse("world", anychar ~ failure)) // None
```

**KRISHNA** Sometimes we only need the output from the last parser in the sequence along with the residual input after both the parsers have done their job.  For this purpose, I will define another combination parser to enable this chaining. In place of the second parameter ```q: Parser[U]```, I'll take a function that consumes the output produced by the first parser and returns the second parser.  This chaining is a common operation known as ```bind``` or```flatMap```, depending on which language you are using - I will define this as another infix operator using the Haskell symbol ```>>=```:

```scala
implicit class ParserExtensions[T](p: Parser[T]) {
  ...
  def >>= [U](f: T => Parser[U]): Parser[U] = 
    (in: String) => for {
      (x, out1) <- p(in)
      (y, out) <- f(x)(out1)
    } yield (y, out)
}

println(parse("world", anychar >>= (x => success(x)))) // Some((w,orld))
println(parse("world", anychar >>= (x => failure))) // None
```

**KRISHNA**  Just like the earlier ```~``` operator, for ```>>=``` if any one parser in the sequence fails, the entire sequence fails and if all parsers succeed, then the entire sequence succeeds.

**KRISHNA** In addition to parsers like ```anychar```, which we can use to take our string apart, we'll need parsers that validate whether a part of the string satisfies a particular constraint, like being a letter or say a digit.  To generalize - it is about validating whether a character satisfies some kind of test, and fail if it does not.  The ```anychar``` parser consumes a character without any reservations.  Using the above definition, I can define a yet another simple and a useful parser that uses ```anychar``` to consume a character from input while applying any predicate to it.  If the predicate is satisfied, then it returns that character else it simply fails. Lets call this parser ```satisfy```:  

```scala
def satisfy(pred: Char => Boolean): Parser[Char] = 
  anychar >>= (ch => if (pred(ch)) success(ch) else failure)
    
println(parse("hello", satisfy(_ == 'h'))) // Some((h,ello))
println(parse("world", satisfy(_ == 'h'))) // None
println(parse("", satisfy(_ == 'h')))      // None
```

**KRISHNA**  We are nearly there: using the ```satisfy``` parser, I can now build a parser that consumes a specific character from the input string and rejects everything else:

```scala
def char(ch: Char) = satisfy(ch == _)

println(parse("[a]", char('['))) // Some(([,a]))
println(parse("{2}", char('['))) // None
```

**KRISHNA** Likewise, using the satisfy parser, I can also build a letter and a digit parser:

```scala
def letter = satisfy(Character.isLetter _)
def digit = satisfy(Character.isDigit _)

println(parse("hello", letter)) // Some((h,ello))
println(parse("12345", digit)) //  Some((1,2345))
println(parse("12345", letter)) // None
```

**KRISHNA**  In addition to sequences, I need to create a combinator which will allows one or more alternatives, like a letter or digit. To define this ```orElse``` combinator, I'll use the ```|```  from BNF, so I will be able to write:

```scala
def alphanum = letter | digit
```

**KRISHNA**  The ```|``` combinator will apply the first parser, and if it  fails, try the second parser ```q``` on the same input and return the output of the second parser.  In case the first succeeds, it returns the output from the first and does not evaluate the second. You can probably guess what this is going to look like now:

```scala
implicit class ParserExtensions[T](p: Parser[T]) {
  ...
  ...    
  def | [U >: T](q: Parser[U]): Parser[U] = 
    (in: String) => p(in) match {
      case None => q(in)
      case out  => out
    }
}

println(parse("a1", alphanum)) // Some((a,1))
println(parse("1a", alphanum)) // Some((1,a))
println(parse("!!", alphanum)) // None
```

**KRISHNA**  The last step is to be able to convert extracted sections of our parsed string to other types. For example, we really want the digit parser to return an integer to be able to something useful with it. So let's introduce a mapping operation on the parser that consumes a function which maps the input to the required output type.  I'll use the symbol ```^^``` double caret for that:

```scala
implicit class ParserExtensions[T](p: Parser[T]) {
  ...    
  ...
  def ^^ [U](f: T => U): Parser[U] = 
    p >>= (t => success(f(t)))
}
```

**KRISHNA** The above ```map``` is implemented in terms of ```>>=```, AKA ```bind``` or```flatMap```.  If ```p``` succeeds, then the value is wrapped in a ```success``` parser. I can now use the above ```digit``` parser as:

```scala
def digitAsInt: Parser[Int] = digit ^^ (ch => Integer.parseInt(ch.toString))

println(parse("3", digitAsInt)) // Some((3,)) type: Option[(Int, String)]
println(parse("a", digitAsInt)) // None       type: Option[(Int, String)]
```

**KRISHNA** You will remember that our goal is to create an embedded DSL which is very similar to BNF grammar - a language for defining a language, wherein it composes bigger grammar from smaller grammer.  The BNF/EBNF grammar for an array containing either a digit or a letter can be written:

```
array ::= "[" [ value ] "]".
value ::= letter | digit
```

**KRISHNA**  Now, I can parse an array with single element using syntax which is quite similar to BNF:

```scala
println(parse("[a]", char('[') ~ (letter | digitAsInt) ~ char(']'))) // Some(((([,a),]),))
```

**KRISHNA**  If you look at the output, I'm really not interested in returning the square brackets, I'd like to drop them, so that I can simply extract the value.  For this, I'll write combinators to "drop left" (or take right) ```~>``` and "drop right" (or take left) ```<~```.

```scala
implicit class ParserExtensions[T](p: Parser[T]) {
  ...    
  ...
  def ~> [U](q: Parser[U]): Parser[U] = 
    p~q ^^ { case (x, y) => y }
    
  def <~ [U](q: Parser[U]): Parser[T] = 
    p~q ^^ { case (x, y) => x }
}

println(parse("[a]", char('[') ~> (letter | digitAsInt) <~ char(']'))) // Some((a,))
```

**KRISHNA**  Using this, I can start shaping an array parser like this. We're getting very close to BNF:

```scala
object ParseArray {

  def value = letter | digitAsInt
  def array = char('[') ~> value <~ char(']')

  def apply(in: String) = parse(in, array)
}

println(ParseArray("[a]")) // Some((a,))
```

**KRISHNA**  So this is how we can parse singleton arrays in a functional programming paradigm. 

**BRAHMA**  That's very cool indeed. You wrote quite a bit more code than I did, but I can see the attraction of a general framework. I have seen APL programmers use similar techniques when writing parsers, but many do not have computer science backgrounds and are unaware of such techniques. I'm going to stick with my extreme / minimalistic approach because it is a more natural expression of the array paradigm. I hope I can make it through all three verses without regretting that too much, I am feeling very grateful that we decided we didn't have enough time today to parse a more complex grammar (however, check the GitHub repo for a complete JSON parser - it is not too hideous, IMHO).

**BRAHMA**  The next verse is a parser for arrays containing more than one item, for example ```[a,2,z]```. You may remember the singleton array parser in APL looked like this:

```apl
    ArrayParser←{
       '[]'≢(⊣/,⊢/)⍵ : 'missing outer []' ⎕SIGNAL 11 
       1<≢inner←1↓¯1↓⍵ : 'max one item allowed' ⎕SIGNAL 11
       ⊃inner∊⎕D: ⎕D⍳inner
       inner
     }
```

**BRAHMA** The code to parse arrays with more than one item looks like this - 6 lines of code instead of 4:

```apl
    ArrayParser←{            ⍝ Convert string representation to an APL vector
       '[]'≢(⊣/,⊢/)⍵: 'missing outer []' ⎕SIGNAL 11 
       inner←1↓¯1↓⍵                   ⍝ drop "[]"
       items←{1↓¨(⍵=',')⊂⍵} ',',inner ⍝ cut on ","
       1∨.≠≢¨items: 'only one char allowed per item' ⎕SIGNAL 11
       (⎕D∘⍳)@(∊∘⎕D) ∊values ⍝ replace digits by corresponding integers
    } 
```

**BRAHMA**  The first line which validates that we have the expected square brackets, is unchanged.  In the second line, I still drop the first and last character, but no longer validate the total length of ```inner```, because we might have more than one item.  Instead, I will break ```inner``` up into items using the partition primitive ```⊂```, using each occurrence of a comma start a new partition (an leading comma is added in order to start the first segment). After the partition, the leading commas in each item are removed using ```1↓¨``` (1 drop each).

**BRAHMA**  If any of the item lengths are not equal to 1, an exception is thrown. We use the enlist function ```(∊)``` to turn the array, which is a list of 1-element lists, into a simple list of characters.

Finally, we use the "at" operator ```(@)``` to selectively apply the same type cast as in the singleton case - but only to those characters which are digits (satisfy the constraint ```(∊∘⎕D)```).  The ```@``` operator takes two function operands: a transformation on the left and a selector on the right. It returns a result which is a copy of the right argument, with the transformation applied to the selected items.

**BRAHMA**  Krishna, I am looking forward to seeing how you will extend your parser framework to allow multiple array elements!

**KRISHNA**  Yes, I am going to need another parser combinator, which will allow me to express that a pattern might be repeated more than once - and if this pattern is found, return the items as a list of elements.  In Extended BNF (EBNF), a postfix ```*``` means that a pattern can be repeated any number of times.  Fortunately, Scala allows me to do exactly what I want; I can create a ```many``` parser as a postfix symbol ```*```.  

```scala
implicit class ParserExtensions[T](p: Parser[T]) {
  ...
  ...
  import scala.language.postfixOps  
  def * : Parser[List[T]] = 
    p~(p*) ^^ { case (x, xs) => x :: xs } | success(List())
}
```

**KRISHNA**  Above, I apply the parser ```p``` once and use ```~``` to call ```p*``` recursively on what remains - and map ```^^``` the output by consing (```::```) ```x``` (returned from calling ```p```)  with ```xs``` (returned from calling ```p*```) .  As ```*``` is applied postfix, I need to add the line ```import scala.language.postfixOps``` else I'll get a warning message from the compiler.

**KRISHNA** Further, I need to make sure that for the sequential ```~``` combinator, I use the call-by-name semantics denoted by ```=>``` for the recursive call.  A call-by-name argument will be evaluated only when it is needed, which is only after the first call to ```p```. If call-by-value semantics is used, it will cause stack overflow without reading any input, when calling the many ```*``` combinator.

**KRISHNA** Finally, I'll have to add the base case for recursion, i.e., for empty string I'd ```|``` with ```success(List())``` parser.  

**KRISHNA** Now that I have the ```many``` parser, I can parse a sequence of letters:

```scala
println(parse("hi", letter*)) // Some((List('h','i'),))
```

**KRISHNA**  Now, our array elements are separated by a comma ```,``` delimiter, so I'll create an overloaded version of the many ```*``` parser that takes in a separator parser as a parameter.  Also, just as in the earlier case, I had to take the second parser as call-by-name, so will I do in the ```~>``` combinator.  For symmetry, I'll also do it in ```<~``` combinator.

```scala
implicit class ParserExtensions[T](p: Parser[T]) {
  ...
  ...
  def ~> [U](q: => Parser[U]): Parser[U] = 
    p~q ^^ { case (x, y) => y }
    
  def <~ [U](q: => Parser[U]): Parser[T] = 
    p~q ^^ { case (x, y) => x }

  def * (sep: Parser[Any]): Parser[List[T]] = 
    p~(sep~>p*) ^^ { case (x, xs) => x :: xs } | success(List())
}
```

**KRISHNA** Above, I used the right ```~>``` combinator to drop the separator from the output, the rest of the definition is same as the ```*``` many combinator.  Now, I can write the definition of an array parser which allows multiple array items as:

```scala
object ParseArray {

  def value: Parser[Any] = letter | digitAsInt
  def array: Parser[List[Any]] = char('[') ~> (value*(char(','))) <~ char(']')
  
  def apply(in: String) = parse(in, array)
}

println(ParseArray("[a,1]")) // Some((List(a,1),)
```

**KRISHNA**  So this is how we can parse multiple array elements with a separator/delimiter.  The final verse we're going to play is to enhance our parsers to support nested arrays. How will you do that in APL?

**BRAHMA**  My strategy to parse items is based on using the APL partition function with a boolean vector marking the beginning of each item on the left, and the string to be cut into pieces on the right. In the simple array case, I could simply base the partitioning on comparing the charcters in our string to comma:

```apl
     inner←'a,9,z'    ⍝ a 3-element aray (brackeys removed)
     {1↓¨(','=⍵)⊂⍵}',',inner ⍝ prepend one comma, cut, drop leading commas
┌─┬─┬─┐
│a│9│z│
└─┴─┴─┘
```

**BRAHMA** Now, with nested arrays, I have the problem that some of the commas are inside nested cells, and should not be used to cut the array at the top level. Fortunately, it is not too hard to mask commas which are inside inner brackets, by computing the level of nesting of brackets. This can be done by looking each character up in the list '[]', mapping these two characters to +1 and -1, respectively (and anything else to 0), and doing a sum scan of the result:

```apl
     inner←'a,9,[b,[c,4]]'
     depth←+\1 ¯1 0['[]'⍳inner]
     ↑inner (1 0⍕depth) ⍝ format depth width 1, decimals 0
a,9,[b,[c,4]]
0000111222210
```

**BRAHMA** The last expression above displays inner and the computed depth of nesting. We can see that the depth increases by 1 at each "[" and decreases at each "]". In order to break the representation up at the top level, I only want to break on commas where the depth of nesting is 0:

```apl
    here←depth=0
    {1↓¨(1,here∧⍵=',')⊂',',⍵} inner
┌─┬─┬─────────┐
│a│9│[b,[c,4]]│
└─┴─┴─────────┘
```

**BRAHMA**  Once I have broken the representation up in this way, I can call the parser recursively on any item which begins with "[", and apply the same processing to the other items as for the simple array case.  I've decided to break the parsing or "leaf" items out into a separate function called ```LeafParser```. The complete solution for nested parsing can be written as follows:

```apl
     LeafParser←{             ⍝ parse leaves (single letters or digit)
        1∨.≠≢¨⍵ : 'only one char allowed per item' ⎕SIGNAL 11
        (⎕D∘⍳)@(∊∘⎕D) ∊⍵     ⍝ replace digits by corresponding integers
     }
     
     ArrayParser←{   ⍝ Convert string representation to a nested array  
        '[]'≢(⊣/,⊢/)⍵: 'missing outer []' ⎕SIGNAL 11 
        inner←1↓¯1↓⍵                   ⍝ drop []
        here←0=+\1 ¯1 0['[]'⍳inner]    ⍝ (here=0) means not in []
        items←{1↓¨(1,here∧⍵=',')⊂',',⍵} inner ⍝ cut on "," which are not within []
        leaf←~sub←'['=⊃¨items          ⍝ sub-arrays vs leaves
        values←∇¨@(⍸sub)⍣(∨/sub)⊢items ⍝ recursively parse sub-arrays
        LeafParser@(⍸leaf)⊢values      ⍝ parse leaves
      }

     ArrayParser'[a,9,[b,[c,4]],[t,5]]'
┌─┬─┬───────┬───┐
│a│9│┌─┬───┐│t 5│
│ │ ││b│c 4││   │
│ │ │└─┴───┘│   │
└─┴─┴───────┴───┘
```

**BRAHMA**  The 3rd line of code from the bottom computes two masks ```sub``` (which identifies nested items) and ```leaf``` which marks the leaf items. In the final two lines, we use @ to apply the function recursively to the nested items, and ```LeafParser``` to the remaining items. Note that the symbol ```∇``` denotes self-reference, allowing functions to be recursive even if they have not been named. In this case, we could have written ```ArrayParser``` instead of ```∇```, since we have named the function.

**BRAHMA**  So, I managed to get away with it, but at this point I had to make a significant structural change to my code. I presume we're reaching the point where your framework starts to show its true value. How would you do this in Scala?

**KRISHNA**  Indeed! To implement that change, all I need to do is add an ```array |``` to my definition of ```value```, making the whole thing recursive:

```scala
object ParseArray {

  def value: Parser[Any] = array | letter | digitAsInt
  def array: Parser[List[Any]] = char('[') ~> (value*(char(','))) <~ char(']')
  
  def apply(in: String) = parse(in, array)
}

println(ParseArray("[a,1,[ब,२]]")) // Some((List(a,1,List(ब,२)),)
```

**BRAHMA** Lets reflect on this...

Reflections
-----------

**BRAHMA** So, here you broke down the problem of parsing into many small functions, such that each function could stand on it own feet.  At the same time  they could be combined together for any possible permutations to fulfill the requirement.  The focus was in doing small and avoid over-complicated monolith code.

**KRISHNA** Yes, smaller functions provide the simplicity of understanding the code in chunks and help to build a big picture.  This stems from the premise that this clarity helps comprehension and it becomes relatively easy facilitate local changes when the need be.

**BRAHMA** In APL, we tend to take a macro view of the problem. 
//TODO: Morten to fill in...
 

**BRAHMA** 
