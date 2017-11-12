 FizzBuzz←{
     primes←3 5
     text←'Fizz' 'Buzz' 'FizzBuzz'
     case←2⊥⊖0=primes∘.|⍵ ⍝ 0=nothing, 1=Fizz, 2=Buzz, 3=FizzBuzz
     (messages[case~0]@(⍸case≠0))⍵
 }
