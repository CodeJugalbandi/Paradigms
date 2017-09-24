The Luhn Algorithm is a simple checksum formula used to validate a variety of identification numbers, such as credit card numbers:

[https://en.wikipedia.org/wiki/Luhn_algorithm](https://en.wikipedia.org/wiki/Luhn_algorithm "Wikipedia: Luhn Algorithm")

* Reverse the order of the digits in the number.
* Take the first, third, ... and every other odd digit in the reversed digits and sum them to form the partial sum s1
* Taking the second, fourth ... and every other even digit in the reversed digits:
    * Multiply each digit by two and sum the digits if the answer is greater than nine to form partial sums for the even digits
    * Sum the partial sums of the even digits to form s2
* If s1 + s2 ends in zero then the original number is in the form of a valid credit card number as verified by the Luhn test.

```
For example, if the trial number is 49927398716:

Reverse the digits:
  61789372994
Sum the odd digits:
  6 + 7 + 9 + 7 + 9 + 4 = 42 = s1
The even digits:
    1,  8,  3,  2,  9
  Two times each even digit:
    2, 16,  6,  4, 18
  Sum the digits of each multiplication:
    2,  7,  6,  4,  9
  Sum the last:
    2 + 7 + 6 + 4 + 9 = 28 = s2

s1 + s2 = 70 which ends in zero which means that 49927398716 passes the Luhn test
```

```apl
    creditCardAsString ← '49927398716'
    creditCard ← (⎕UCS creditCardAsString)-48
    reversed ← ⌽creditCard
⍝ 61789372994

    ⍴⌽reversed
⍝ 11

    indices ← ⍳⍴⌽reversed    
⍝ 1 2 3 4 5 6 7 8 9 10 11

    2|indices
⍝ 1 0 1 0 1 0 1 0 1 0 1
  
    0=2|indices
⍝ 0 1 0 1 0 1 0 1 0 1 0

    oddIndices ← ⍸2|indices
⍝ 1 3 5 7 9 11
  
    evenIndices ← ⍸0=2|indices
⍝ 2 4 6 8 10

    oddSum ← +/reversed[oddIndices]
    evenSum ← +/+/⍉10 10⊤2×reversed[evenIndices]
    isLuhn ← 0=10|(evenSum + oddSum)
      
```