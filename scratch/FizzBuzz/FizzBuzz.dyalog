 FizzBuzz←{
     primes←3 5
     text←'Fizz' 'Buzz' 'FizzBuzz'
     case←2⊥⊖0=primes∘.|⍵ ⍝ 0=nothing, 1=Fizz, 2=Buzz, 3=FizzBuzz
     (messages[case~0]@(⍸0≠case))⍵
 }
⍝)(!FizzBuzz!mkrom!2017 11 2 15 2 16 0!0
