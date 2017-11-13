 FizzBuzz←{
     primes←3 5
     text←'Fizz' 'Buzz' 'FizzBuzz'
     case←2⊥⊖0=primes∘.|⍵ ⍝ 0=nothing, 1=Fizz, 2=Buzz, 3=FizzBuzz
     (text[case~0]@(⍸case≠0))⍵
 }
⍝)(!FizzBuzz!mkrom!2017 11 13 6 54 39 0!0
