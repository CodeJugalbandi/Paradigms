:Namespace Luhn

    Calculate←{10|-+/,0 10⊤⍵×⌽(≢⍵)⍴2 1}  ⍝ calculate the check digit
    Verfiy←{(⊃⌽⍵)≡Calculate ¯1↓⍵}        ⍝ verify the check digit

:EndNamespace
