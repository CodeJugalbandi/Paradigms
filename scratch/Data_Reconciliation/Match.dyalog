:Namespace Match

    writeout←{
      data←⊃⍪/(⍳≢⍺),¨⍺[;1],⍤1¨⍺[;2]
      data(⎕CSV⍠'Overwrite' 1) ⍵
     } ⍝ join id, keys with data for outfile

      Run←{
          folder←⍵,(0=≢⍵)/'C:\Devt\Paradigms\scratch\Data_Reconciliation\' ⍝ Default folder
          (source mirror)←{⎕CSV folder,⍵,'.csv'}¨'source' 'mirror'         ⍝ read CSV files
          (matched recs)←source Compare mirror                             ⍝ Do comparison
          z←((,matched)⌿recs)writeout folder,'matched.csv'                 ⍝ Write file with matched recs
          z←((~,matched)⌿recs)writeout folder,'mismatched.csv'             ⍝ File with mispatched recs
      }

      Compare←{
          ⍝ ⍺ is source and ⍵ is mirrored records
          ⍝ data[;1] zip, [;2] name, [;3] amount, [;4] mirror code, [;5] s/m  

          data←⍺(,⍤1)⊂''          ⍝ Add empty key
          data⍪←⍵[;1 3 2 4]       ⍝ Reorder columns and add 'm'
     
          data[;3]←2⊃⎕VFI(' '@(=∘'$'))∊data[;3]  ⍝ Make amounts numeric ignoring '$'
          data[;3]×←¯1*(⍳≢data)>≢⍺               ⍝ Multiply all mirror values by ¯1
     
          1 1 0⊂data[;1 2]{(0=+/⍵[;1]),⍺ ⍵}⌸0 2↓data ⍝ match flag, unique zip/name pairs, corresponding records
      }

:EndNamespace
