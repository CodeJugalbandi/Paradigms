:Namespace Parsing                                        

    ⎕IO←0

      FromJSON←{                      ⍝ Convert JSON string to APL array or object
     
          quoted←{{⍵∨¯1⌽⍵}≠\⍵='"'}    ⍝ find quoted sections of a string
     
          collection←{                ⍝ evaluate an array or object
              ~(⊂(⊃,(⊃⌽))⍵)∊'[]' '{}':'Unbalanced brackets/braces'⎕SIGNAL 11
              json←1↓¯1↓⍵                           ⍝ drop brackets/braces
              notqtd←~quoted json                   ⍝ not quoted parts
              here←0=+\notqtd×1 ¯1 1 ¯1 0['{}[]'⍳json] ⍝ ... and not in brackets/braces
              split←(notqtd∧here)∧','=json          ⍝ , splits
              ('{'=⊃⍵)collect Eval¨1↓¨(1,split)⊂',',json ⍝ recurse on each part
          }
     
          collect←{⍺=0:⍵ ⍝ Array can be returned as is
                    ⍝ Member name/value pairs need to become variables in a Namespace
              z←(ns←⎕NS'').⎕DF'[Object]'            ⍝ create namespace
              ~∧/m←¯1≠⊃∘⎕NC¨⊃¨⍵:('Unsupported name(s): ',,⍕m/⊃¨⍵)⎕SIGNAL 11
         ⍝ ↑↑↑ APL cannot support all JSON names
              ns⊣ns.{⍎(0⊃⍵),'←1⊃⍵'}¨⍵               ⍝ inject names into space
          }
     
          value←{ ⍝ Handle a single value / member
              qtd←quoted ⍵
              ∧/qtd:1↓¯1↓⍵            ⍝ If all quoted, it's a string
              (valid num)←⎕VFI ('-'⎕R'¯')⍵ ⍝ validate ⍵ as a number
              (,1)≡valid:⊃num         ⍝ If a single valid number, return that
              ':'=(qtd⍳0)⊃⍵:member ⍵  ⍝ If 1st unquoted char is :, it is a single member
              3≠i←'true' 'false' 'null'⍳⊂⍵:i⊃1 0 ⎕NULL ⍝ boolean ⍝ Special names
              ('Invalid value: ',⍵)⎕SIGNAL 11
          }
     
          member←{
              notqtd←~quoted ⍵       ⍝ not quoted parts
              colon←(notqtd∧⍵=':')⍳1 ⍝ find the colon
              name←1↓(colon-1)↑⍵     ⍝ drop the "
              val←Eval(colon+1)↓⍵    ⍝ Evaluate the data
              name val
          }
     
          Eval←{                     ⍝ Evaluate a JSON array, collection, value or member
              (⊃⍵)∊'[{':collection ⍵
              value ⍵
          }
     
          json←((⍵≠' ')∨quoted ⍵)/⍵    ⍝ remove unquoted blanks
          ~(⊃json)∊'{[':'JSON does not represent an array or object'⎕SIGNAL 11
          Eval json
      }
    
    :Section Tests
      
    assert←{0∊⍵: ('Conversion failed on: ',⍺) ⎕SIGNAL 11}
    checkError←{
       ~∨/⍵⍵⍷⍺⍺:('Expected error "',⍵⍵,'", got "',⍺⍺,'" for: ',⍵) ⎕SIGNAL 11
       }
    catch←{
      0::(⎕DMX.EM checkError ⍵⍵)⍵
      ⍺⍺ ⍵ 
      1÷0}

    ∇ Test;json;o;then;now     
      then←⎕AI[3]
      
     ⍝ Tests which should succeed 
      ('simple array: ',json)assert(1 0 ⎕NULL ¯1 3.1415)≡FromJSON json←'[true, false, null, -1, 3.1415]'
      ('simple object: ',json)assert('hello' 42)≡(FromJSON json←'{"greeting":"hello", "life":42}').(greeting life)
      o←FromJSON json←'[3.14,{"greeting":"hello", "thing":[{"life":42},false]}]'
      ('nested: ',json)assert(3.14 'hello' 42 0)≡(o[0])(o[1].greeting)(o[1].thing[0].life)(o[1].thing[1])

     ⍝ Tests which should fail      
      (FromJSON catch 'JSON does not represent an array or object')'2.2'
      (FromJSON catch 'Invalid value')'[trueish,falseish]'
      (FromJSON catch 'Unbalanced brackets/braces')'[1,2,3}'
      (FromJSON catch 'Unsupported name(s):')'{"delta y":3}'
      (FromJSON catch 'Invalid value')'{"y":3:4}'     

      now←⎕AI[3]      
      ⎕←'Tests passed in',(⍕1⍕now-then),'s'
    ∇          

    :EndSection

:EndNamespace
⍝)(!Test!mkrom!2017 10 4 20 42 47 0!0
