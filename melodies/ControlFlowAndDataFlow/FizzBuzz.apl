input ← ⍳20
      3∘.|input
⍝ 1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2
      5∘.|input
⍝ 1 2 3 4 0 1 2 3 4 0 1 2 3 4 0 1 2 3 4 0
      5 3∘.|input
⍝ 1 2 3 4 0 1 2 3 4 0 1 2 3 4 0 1 2 3 4 0
⍝ 1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2 0 1 2
      0=5 3∘.|input
⍝ 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1
⍝ 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0
      select ← 2⊥0=5 3∘.|input
⍝ 0 0 1 0 2 1 0 0 1 2 0 1 0 0 3 0 0 1 0 2
      indices ← ⍸select≠0
⍝ 3 5 6 9 10 12 15 18 20
      select[indices]
⍝ 1 2 1 1 2 1 3 1 2
    'Fizz' 'Buzz' 'FizzBuzz'[select[indices]]
⍝ Fizz  Buzz  Fizz  Fizz  Buzz  Fizz  FizzBuzz  Fizz  Buzz

    input[indices] ← 'Fizz' 'Buzz' 'FizzBuzz'[select[indices]]
    input
⍝ 1 2  Fizz  4  Buzz  Fizz  7 8  Fizz  Buzz  11  Fizz  13 14  FizzBuzz  16
       17  Fizz  19  Buzz 

input ← ⍳20
fivesAndThrees ← 0=5 3∘.|input
select ← 2⊥fivesAndThrees
indices ← ⍸select≠0
input[indices] ← 'Fizz' 'Buzz' 'FizzBuzz'[select[indices]]
input

