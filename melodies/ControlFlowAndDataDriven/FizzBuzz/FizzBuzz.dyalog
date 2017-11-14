 FizzBuzz←{
     primes←3 5 7
     text←'' 'Fizz' 'Buzz' 'FizzBuzz' 'Bleep' 'FizzBleep' 'BuzzBleep' 'FizzBuzzBleep'
     case←2⊥⊖0=primes∘.|⍵ ⍝ 0=nothing, 1=Fizz, 2=Buzz, 3=FizzBuzz
     (text[case~0]@(⍸case≠0))⍵
 }
