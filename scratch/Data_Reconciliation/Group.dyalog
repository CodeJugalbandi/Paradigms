:Namespace Group
⍝ Experiments with prefix/suffix based grouping in APL
      
      ⎕IO←0
      keys←,¨'DK' 'B' 'A' 'DK' 'UK' 'DK' 'US1' 'F' 'USA' 'USAF' 'US2' 'UK' 'USA' 'DK' 'UK1' 'FAM'
      keys←,¨'DK' 'B' 'A' 'DK1' 'UK' 'DK' 'US1' 'F' 'USA' 'USAF' 'US2' 'UK' 'FAM' 'UMM'

      afx←{
          ⍺←1                         ⍝ 1 for prefix; ¯1 for suffix
          z←⍳≢m←≢¨⍵
          afx1←{                      ⍝ do prefixes of length ⍺
              b←m=|⍺                    ⍝ strings exactly ⍺ chars long
              c←m>|⍺                    ⍝ strings which could have prefixes with ⍺ chars
              x←x[i←⍋x←↑(b/⍵),⍺↑¨c/⍵;]
              d←((≢x)↑1)∨∨/x≠¯1⊖x       ⍝ where a row differs from previous row
              p←d∧i<+/b                 ⍝ demarcate prefix groups
              s←⍳∘0¨p⊂d≤p               ⍝ in each group, ≢ of prefix matches in each group
              j←s↑¨p⊂((⍸b),⍸c)[i]       ⍝ in each group, indices in original ⍵
              q←s↑¨p⊂i<+/b              ⍝ in each group, select indices of prefixes
              z[∊j],←(≢¨j)/q/¨j
              0
          }
          z⊣(⍺×∪m)afx1¨⊂⍵
      }

      affix←{    ⍝ Morten's prefix/suffix problem, 2017-10-16
          p←1 afx⍤1⍉⍵
          s←¯1 afx⍤1⍉⍵
          ∩⌿flip⍤1⊢p∪¨s
      }

      flip←{    ⍝ if x≈y then y≈x
          {⍵[⍋⍵]}¨∪¨((∊⍵){⊂⍵}⌸(≢¨⍵)/⍳≢⍵),¨@(∪∊⍵)⊢⍵
      }
      

     test←{
      i←affix ⍪keys
      ⍕ (⍳≢keys),keys,i, ⍪(⊂¨i)⌷¨⊂keys
     }
:EndNamespace
