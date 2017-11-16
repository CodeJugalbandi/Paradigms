 FizzBuzz←{
     primes←3 5
     words←'' 'Fizz' 'Buzz' 'FizzBuzz'
     case←2⊥⊖0=primes∘.|⍵
     words[case~0]@{case≠0} ⍵
 }
