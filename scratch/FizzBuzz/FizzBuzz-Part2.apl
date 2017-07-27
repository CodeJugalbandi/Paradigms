⍝ Convert a number in to vector
number ← 564
10⍟number       
⍝ 2.751279104
      1+10⍟number
⍝ 3.751279104
      digits ← ⌊1+10⍟number ⍝ Do Floor(1 + Log N) to get number of digits
⍝ 3
vector ← (digits⍴10)⊤number ⍝ encode number by re-shaping the digits using base 10. 
⍝ 5 6 4 

⍝ Generating up to 20
N ← 20

⍝ Below implementation uses each operator with anonymous function inside it
⍝ to calculate indicesFor3 

input ← ⍳N
fivesAndThrees ← 0=5 3∘.|input
select ← 2⊥fivesAndThrees
indices ← ⍸select≠0
indicesFor3 ← ⍸{⍬≢(((⌊1+10⍟⍵)⍴10)⊤⍵)∩3} ¨ input
input[indices] ← 'Fizz' 'Buzz' 'FizzBuzz'[select[indices]]
input[indicesFor3] ← 'Lucky' '' [1]


⍝ Below implementation is 100% data-driven and calculates indicesFor3 

input ← ⍳N
fivesAndThrees ← 0=5 3∘.|input
select ← 2⊥fivesAndThrees
indices ← ⍸select≠0
vector ← ((⌊1+10⍟input)[N]⍴10)⊤input
indicesFor3 ← ⍸0≠2⊥(3∘.=vector)
input[indices] ← 'Fizz' 'Buzz' 'FizzBuzz'[select[indices]]
input[indicesFor3] ← 'Lucky' '' [1]