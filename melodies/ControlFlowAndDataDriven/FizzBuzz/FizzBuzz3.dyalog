 FizzBuzz3←{
     primes←3 5 7
     words←'' 'Fizz' 'Buzz' 'FizzBuzz' 'Bleep' 'FizzBleep' 'BuzzBleep' 'FizzBuzzBleep'
     case←2⊥⊖0=primes∘.|⍵
     words[case~0]@{case≠0} ⍵
 }
