rove←{(position heading commands)←⍵             ⍝ Deconstruct right argument
      directions←'NESW'                         ⍝ Clockwise from N
      movement←4 2⍴0 1,1 0,0 ¯1,¯1 0            ⍝ One row per direction
      direction←directions⍳heading              ⍝ index from heading char
      bearings←4|+\direction,¯1+'LMR'⍳commands  ⍝ Bearing after each command
      moves←movement[¯1↓bearings;]×[0]commands='M' ⍝ Movement for each cmd
      (+⌿position⍪moves),directions[¯1↑bearings] ⍝ Final coords and bearing
     }
