:Namespace ParsingAtom

      ⎕IO←0

      ArrayParser←{           ⍝ Convert string representation to APL array with 0 or 1 elements
          '[]'≢(⊣/,⊢/)⍵: 'missing outer []' ⎕SIGNAL 11 
          1<≢inner←1↓¯1↓⍵ : 'max one item allowed' ⎕SIGNAL 11 ⍝ drop "[]"
          ⊃inner∊⎕D: ⎕D⍳inner  ⍝ replace digit by corresponding integer
          inner
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
      ('one integer: ',txt)assert(,2)≡ArrayParser txt←'[2]'
      ('empty array: ',txt) assert ''≡ArrayParser txt←'[]'
     
     ⍝ Tests which should fail
      (ArrayParser catch'max one item')'[ab]'
      (ArrayParser catch'missing outer []')'a,1'
     
      now←⎕AI[3]
      ⎕←'Tests passed in',(⍕1⍕now-then),'s'
    ∇

   :EndSection

:EndNamespace
⍝)(!Test!mkrom!2017 10 25 19 59 1 0!0
