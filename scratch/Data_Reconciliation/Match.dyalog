 r←Match folder;source;mirror;data
⍝ Perform transaction matching (but with exact name matches)
⍝ As described in https://www.slideshare.net/DhavalDalal/data-reconciliation

 folder,←(0=≢folder)/'C:\Devt\Paradigms\scratch\Data_Reconciliation\'
 source←⎕CSV folder,'source.csv'
 mirror←⎕CSV folder,'mirror.csv'

 data←(source,⊂''),'s'
 data⍪←mirror[;1 3 2 4],'m'
⍝ data[;1] zip, [;2] name, [;3] amount, [;4] mirror code, [;5] s/m

 data[;3]←2⊃⎕VFI(' '@(=∘'$'))∊data[;3]   ⍝ Make numeric
 data[;3]×←¯1*'m'=data[;5]               ⍝ Make all mirror values negitiv

 r←data[;1 2]{(⍺,+/⍵[;1])⍵}⌸0 2↓data     ⍝ keys, OK flag, corresponding records
