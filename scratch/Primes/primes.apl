n ← 13
      ⍳n
⍝ 1 2 3 4 5 6 7 8 9 10 11 12 13

  (⍳n)∘.|⍳n
⍝ 0 0 0 0 0 0 0 0 0  0  0  0 0
⍝ 1 0 1 0 1 0 1 0 1  0  1  0 1
⍝ 1 2 0 1 2 0 1 2 0  1  2  0 1
⍝ 1 2 3 0 1 2 3 0 1  2  3  0 1
⍝ 1 2 3 4 0 1 2 3 4  0  1  2 3
⍝ 1 2 3 4 5 0 1 2 3  4  5  0 1
⍝ 1 2 3 4 5 6 0 1 2  3  4  5 6
⍝ 1 2 3 4 5 6 7 0 1  2  3  4 5
⍝ 1 2 3 4 5 6 7 8 0  1  2  3 4
⍝ 1 2 3 4 5 6 7 8 9  0  1  2 3
⍝ 1 2 3 4 5 6 7 8 9 10  0  1 2
⍝ 1 2 3 4 5 6 7 8 9 10 11  0 1
⍝ 1 2 3 4 5 6 7 8 9 10 11 12 0

  0=(⍳n)∘.|⍳n
⍝ 1 1 1 1 1 1 1 1 1 1 1 1 1
⍝ 0 1 0 1 0 1 0 1 0 1 0 1 0
⍝ 0 0 1 0 0 1 0 0 1 0 0 1 0
⍝ 0 0 0 1 0 0 0 1 0 0 0 1 0
⍝ 0 0 0 0 1 0 0 0 0 1 0 0 0
⍝ 0 0 0 0 0 1 0 0 0 0 0 1 0
⍝ 0 0 0 0 0 0 1 0 0 0 0 0 0
⍝ 0 0 0 0 0 0 0 1 0 0 0 0 0
⍝ 0 0 0 0 0 0 0 0 1 0 0 0 0
⍝ 0 0 0 0 0 0 0 0 0 1 0 0 0
⍝ 0 0 0 0 0 0 0 0 0 0 1 0 0
⍝ 0 0 0 0 0 0 0 0 0 0 0 1 0
⍝ 0 0 0 0 0 0 0 0 0 0 0 0 1

  +⌿0=(⍳n)∘.|⍳n
⍝ 1 2 2 3 2 4 2 4 3 4 2 6 2
    
  2=+⌿0=(⍳n)∘.|⍳n
⍝ 0 1 1 0 1 0 1 0 0 0 1 0 1  

  ⍸2=+⌿0=(⍳n)∘.|⍳n
⍝ 2 3 5 7 11 13

]RUNTIME ⍸2=+⌿0=(⍳n)∘.|⍳n

* Benchmarking "⍴⍸2=+⌿0=(⍳n)∘.|⍳n"
             (ms) 
 CPU (avg):   999 
 Elapsed:    1005 
