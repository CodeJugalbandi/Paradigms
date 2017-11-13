:Namespace ParsingArrayNested

      ⎕IO←0

      ArrayParser←{   ⍝ Convert string representation to a nested array  
          '[]'≢(⊣/,⊢/)⍵: 'missing outer []' ⎕SIGNAL 11 
          inner←1↓¯1↓⍵                   ⍝ drop []
          here←0=+\1 ¯1 0['[]'⍳inner]    ⍝ (here=0) means not in []
          items←{1↓¨(1,here∧⍵=',')⊂',',⍵} inner ⍝ cut on "," which are not within []
          leaf←~sub←'['=⊃¨items          ⍝ sub-arrays vs leaves
          values←∇¨@(⍸sub)⍣(∨/sub)⊢items ⍝ recursively parse sub-arrays
          LeafParser@(⍸leaf)⊢values      ⍝ parse leaves
      }         

      LeafParser←{             ⍝ parse leaves (single letters or digit)
          1∨.≠≢¨⍵ : 'only one char allowed per item' ⎕SIGNAL 11
          (⎕D∘⍳)@(∊∘⎕D) ∊⍵     ⍝ replace digits by corresponding integers
      }

   :Section Tests

      assert←{0∊⍵: ('Conversion failed on: ',⍺) ⎕SIGNAL 11}
      checkError←{
          ~∨/⍵⍵⍷⍺⍺:('Expected error "',⍵⍵,'", got "',⍺⍺,'" for: ',⍵)⎕SIGNAL 11
      }
      catch←{
          0::(⎕DMX.EM checkError ⍵⍵)⍵
          z←⍺⍺ ⍵
          1÷0}

    ∇ Test;txt;o;then;now
      then←⎕AI[3]
     
     ⍝ Tests which should succeed
      ('one letter: ',txt)assert(,'a')≡ArrayParser txt←'[a]'
      ('letter and integer: ',txt)assert('a' 1)≡ArrayParser txt←'[a,1]'
      ('nested 1: ',txt)assert('a' (,1))≡ArrayParser txt←'[a,[1]]'
      ('nested 2: ',txt)assert('a' 1 (2 'b'))≡ArrayParser txt←'[a,1,[2,b]]'
     
     ⍝ Tests which should fail
      (ArrayParser catch'only one char')'[a1]'
      (ArrayParser catch'a-z, A-Z, 0-9')'[-,0]'
      (ArrayParser catch'missing outer []')'a,1'
     
      now←⎕AI[3]
      ⎕←'Tests passed in',(⍕1⍕now-then),'s'
    ∇

   :EndSection

:EndNamespace
⍝)(!Test!mkrom!2017 10 29 22 59 49 0!0
