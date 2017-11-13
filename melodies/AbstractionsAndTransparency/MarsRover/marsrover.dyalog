rove←{⎕IO←0
      directions←'NESW'
      movement←4 2⍴0 1,1 0,0 ¯1,¯1 0        ⍝ One row per direction (NESW)    
      (position heading commands)←⍵         ⍝ Deconstruct right argument
      direction←directions⍳heading          ⍝ Direction index
      bearings←4|+\direction,¯1+'LMR'⍳commands   ⍝ Bearing after each command
      moves←movement[¯1↓bearings;]×[0] commands='M' ⍝ Movement resulting from each command
      (+⌿position⍪moves),directions[¯1↑bearings] ⍝ Return final position and bearing
     }
