:Namespace ParsingArrayMinimal

      ⎕IO←0

      ArrayParser←{            ⍝ Convert string representation to array
          ~((⊃,(⊃⌽))⍵)≡'[]':'missing outer []' ⎕SIGNAL 11 
          inner←1↓¯1↓⍵                   ⍝ drop "[]"
          items←{1↓¨(⍵=',')⊂⍵} ',',inner ⍝ cut on ","
          1∨.≠≢¨items: 'only one char allowed per item' ⎕SIGNAL 11
          values←⊃¨items       ⍝ 1-element vectors to scalars
          ~∧/values∊⎕UCS∊48 65 97+⍳¨10 26 26: 'a-z, A-Z, 0-9 only' ⎕SIGNAL 11
          (⎕D∘⍳)@(∊∘⎕D)values  ⍝ replace digits by corresponding integers
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
     
     ⍝ Tests which should fail
      (ArrayParser catch'only one char')'[a1]'
      (ArrayParser catch'a-z, A-Z, 0-9')'[-,0]'
      (ArrayParser catch'missing outer []')'a,1'
     
      now←⎕AI[3]
      ⎕←'Tests passed in',(⍕1⍕now-then),'s'
    ∇

   :EndSection

:EndNamespace
⍝)(!Test!mkrom!2017 10 24 9 4 45 0!0
