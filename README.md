# Hogisim
## Description
Hogisim is a combinational digital logic sandbox where users can construct digital circuits using the seven fundamental logic gates: NOT, OR, AND, XOR, NOR, NAND, XNOR.
Upon opening Hogisim, there are three options. The first option creates an empty sandbox, the second option loads a sandbox from a file, and the third option exits the program.
The board can be populated with multiple instances of fundamental logic gates, an input node which can be toggled to either 0 or 1, or an output node that is updated at execution time.
This program was written as a final project for CSE 230: Principles of Programming Languages taught at UCSD during the Fall 2022 quarter by Professor Nadia Polikarpova.

## Known Issues
- Output nodes cannot be placed adjacent to each other due to the way in which the back-end searches the matrix
