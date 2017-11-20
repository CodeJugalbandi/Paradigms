# Control Flow and Data-Driven Melody

## FizzBuzz Example

FizzBuzz is a mathematical game for children (of all ages), where you are required to count, 1, 2, ... but when you get to a number which is divisible by three, you should say "Fizz" instead of the number. Likewise, you should say "Buzz" for any number which is divisible by five. And of course, "FizzBuzz" for a number which is divisible by both three AND five.

We will attempt to write some code capable of playing FizzBuzz. First, without displaying the numbers - only the words:

Part 1
------
Write code that for a contiguous range of numbers prints out the following:

* 'Fizz' for numbers that are multiples of 3
* 'Buzz' for numbers that are multiples of 5
* 'FizzBuzz' for numbers that are multiples of 15

e.g. Running over a range from 1-20 should give the following output:

```
"" "" Fizz "" Buzz Fizz "" "" Fizz Buzz "" Fizz "" "" FizzBuzz "" "" Fizz "" Buzz
```

Part 2
------
The same as Part 1, except now we want to see the numbers displayed for numbers which are not divisible by either 3 or 5:

```shell
1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz 16 17 Fizz 19 Buzz
```
