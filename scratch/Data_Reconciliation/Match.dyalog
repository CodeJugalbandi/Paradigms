:Namespace Match     

      ⎕IO←0

      Run←{
          folder←⍵,(0=≢⍵)/'C:\Devt\Paradigms\scratch\Data_Reconciliation\' ⍝ Default folder
          (source mirror)←{⎕CSV folder,⍵,'.csv'}¨'source' 'mirror'         ⍝ read CSV files
          (matched recs)←source Compare mirror                             ⍝ Do comparison
          _←((~,matched)⌿recs)writeout folder,'mismatched.csv'             ⍝ File with mismatched recs
          _←((,matched)⌿recs)writeout folder,'matched.csv'                 ⍝ Write file with matched recs
      }

      Compare←{
          ⍝ ⍺ is source and ⍵ is mirrored records
          ⍝ data[;0] zip, [;1] name, [;2] amount, [;3] "mirror code"
     
          data←⍺,⊂''              ⍝ Add empty "mirror code" to source data
          data⍪←⍵[;0 2 1 3]       ⍝ Reorder columns to match
     
          data[;2]←1⊃⎕VFI(' '@(=∘'$'))∊data[;2]  ⍝ Make amounts numeric, ignoring '$'
          data[;2]×←¯1*(⍳≢data)>≢⍺               ⍝ Multiply all mirror values by ¯1
     
          z←data[;0 1]{(0=+/⍵[;0]),⍺ ⍵}⌸0 2↓data ⍝ "match" flag (sum=0), unique zip/name pairs, corresponding records
          exact←(m←z[;0])⌿z 
          rec←Fuzzy (~m)⌿z
          match data←1 1 0⊂exact⍪rec
          data[;2]←Align¨data[;2]
          
      }

      Align←{⍝ Put positive and negative results side by side, padding as necessary
          pos←~neg←0>⍵[;0]      ⍝ negative movements
          max←(+/neg)⌈+/pos     ⍝ maximum of positive or negative recods: number of rows required
          {(max↑neg⌿⍵),max↑pos⌿⍵}(⍕¨|⍵[;0]),⍵[;,1] ⍝ format absolute value of amounts
      }

      writeout←{                ⍝ Write matrix to file in CSV format
          data←⊃⍪/(⍳≢⍺),¨⍺[;0],⍤1¨⍺[;1] ⍝ generate id, join with keys and records
          data(⎕CSV⍠'Overwrite' 1)⍵     ⍝ create CSV, overwriting existing file
      }

      Fuzzy←{  
          groups←affix ⍪1⊃¨⍵[;1] ⍝ See if names are pre/suffixes of each other
          amounts←⊃¨⍵[;2]        ⍝ Extract numeric amounts
          sums←+/¨{amounts[⍵]}¨groups
          zsums←∪(0=sums)/groups ⍝ zero sum groups 
          keys←((≢¨zsums)/(1+⍳≢zsums))@(∊zsums)⊢-⍳≢⍵
          ∘∘∘
          data←
          z←keys{(0=+/nums),(⊂nums←⍵[;0]),⍺(Align ⍵)}⌸0 2↓data ⍝ "match" flag (sum=0), unique zip/name pairs, corresponding records

          ∘∘∘
      }         
      
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

      affix←{⎕IO←0    ⍝ Morten's prefix/suffix problem, 2017-10-16
          p←1 afx⍤1⍉⍵
          s←¯1 afx⍤1⍉⍵
          ∩⌿flip⍤1⊢p∪¨s
      }

      flip←{    ⍝ if x≈y then y≈x
          {⍵[⍋⍵]}¨∪¨((∊⍵){⊂⍵}⌸(≢¨⍵)/⍳≢⍵),¨@(∪∊⍵)⊢⍵
      }


:EndNamespace
