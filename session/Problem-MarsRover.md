# Abstraction and Transparency Melody 

## Mars Rover Example

A robotic rover is to to be landed by NASA on a plateau on Mars. This plateau, which is curiously rectangular, must be navigated by the rover.

A rover's position and location is represented by a combination of x and y co-ordinates and a letter representing one of the four cardinal compass points. The plateau is divided up into a grid to simplify navigation. An example position might be -1, 2, N, which means the rover is one distance unit West, and 2 North of the origin and facing North.

In order to control the rover, NASA sends a simple string of letters. The possible letters are 'L', 'R' and 'M'. 'L' and 'R' makes the rover spin 90 degrees left or right respectively, without moving from its current spot. 'M' means move forward one grid point, and maintain the same heading.

Our task is to write a function which takes as input a starting position and a string containing command letters, and returns the resulting position and orientation of the rover.

For example, for a starting position of (3,3,E) and a command sequence "MMRMMLMRML", the resultr should be (6,0,E).

<center>
<object data="MarsRover.m4v" width="335" height="250"> <param name="src" value="MarsRover.m4v" /> </object>
</center>