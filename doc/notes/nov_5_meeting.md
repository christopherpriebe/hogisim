November 5th, 2022 Group Meeting
================================

Agenda
------
- Discuss potential project ideas
- Choose project idea
- Write high-level description and goals

Ideas
-----
- Some sort of game (Tetris and Brick Breaker are already implemented so not one of those)
- Candy Crush clone
- Card matching game
- Push the box game
- Two player game like Tic-Tac-Toe, Connect 4, Chess 
- Space Invaders clone using words for typing practice
- Spinning cube demo
- CAS like WolframAlpha or SymboLab
- Vim clone
- Procedurally generated birds eye view adventure game
- Virtual playground or sandbox
- Circuit simulator of sorts
- Mandelbrot Zoomer

Pros/Cons
---------

- Two-Player Game: **Pros**: there are a lot of options; no need for CPU player; **Cons**: networking is difficult, would need to do turn-based

- Single-Player Game: **Pros**: there are a lot of options; **Cons**: need to write CPU/game logic

- Sandboxy Interactive Environments: **Pros**: cool and different; **Cons**: large state machine, editing would be hard

Top Choices
-----------
- Sandbox style (digital circuit simulator, powder game style)
- Single-Player Game

Idea
----
**Digital Circuit Simulator**

Next Meeting
-----------
Recurring meeting every Saturday until completion. Ad-hoc meetings if necessary.

Possible Gate Representations
-----------------------------
Option 1: Selection granularity of 3w X h and uses code-style expressions to define logic dates.
Uses either ASCII only (below), or can include some unicode characters to make lines cleaner, though there is something inherently beautiful about ASCII art...
One of the biggest disadvantages here is that these gates cannot really be rotated 90 degrees, only 180 degrees.
This is inspired by https://codegolf.stackexchange.com/questions/22850/ascii-art-logic-gate-diagram   
```
Low Node:
(0)

High Node:
(1)

Horizontal Path:
---

To One Right and One Down:
--\

To One Right and One Up:
--/

To One Right and One Down w/ Cross:
+-\

To One Right and One Up w/ Cross:
+-/

From One Left and One Down :
/--

From One Left and One Up:
\--

Vertical Path:
| 

Path Crossing:
+--

NOT:
<IN>---|>o---<OUT>

AND:
<IN 0>---|&&
         ...
      ---|&&
         |&&---<OUT>
      ---|&&
         ...
<IN N>---|&&

OR:
<IN 0>---|OR
         ...
      ---|OR
         |OR---<OUT>
      ---|OR
         ...
<IN N>---|OR

XOR:
<IN 0>---|^^
         ...
      ---|^^
         |^^---<OUT>
      ---|^^
         ...
<IN N>---|^^

NAND:
<IN 0>---|&o
         ...
      ---|&o
         |&o---<OUT>
      ---|&o
         ...
<IN N>---|&o

NOR:
<IN 0>---|Ro
         ...
      ---|Ro
         |Ro---<OUT>
      ---|Ro
         ...
<IN N>---|Ro

XNOR:
<IN 0>---|^o
         ...
      ---|^o
         |^o---<OUT>
      ---|^o
         ...
<IN N>---|^o

SR-Latch:
<R>---|RQ---<OUT>
      |SR
<S>---|S_

Non-Resetable Flip-Flop:
<IN> ---|DQ---<OUT>
        |FF
<CLK>---|C_

Resetable Flip-Flop:
<IN> ---|DQ---<OUT>
        |FF
<CLK>---|C_
        |FF
<RST>---|R_
```
```
(0)---|>o---|&&
            |&&--\
(1)---|>o---|&&   \--|OR
                     |OR---(0)
(0)---------|&&   /--|OR
            |&&--/
(1)---------|&&    
```
```
(_)---------|DQ---------|DQ---------|DQ---(_) 
            |FF         |FF         |FF
(_)---\-----|C_   /-----|C_   /-----|C-
      |     |FF   |     |FF   |     |FF
(_)---+--\--|R_   |  /--|R_   |  /--|R_
      |  |        |  |        |  |
      |  \--------+-/---------+-/
      \----------/-----------/
```

Option 2: Selection granularity of one character and uses special unicode characters to represent gates and paths.
There are some potential issues with infinite inputs to logic gates and the unicode characters representing the gates are oversized. Inspired by https://lodev.org/logicemu/#id=gates16


Option 3: Use blocks to construct pixel art versions of the gates.
```
NOT Bulb:
--------------|
     ████     |
   ████████   |
 ████████████ |
██████████████|
 ████████████ |
   ████████   |
     ████     |
--------------|

Buffer:
--------------|
██            |
██████        |
██████████    |
██████████████|
██████████    |
██████        |
██            |
--------------|

AND:
--------------| -> Length = 15
████████      |
███████████   |
█████████████ |
██████████████|
█████████████ |
███████████   |
████████      |
--------------| -> Height = 7

OR:
--------------|
███████       |
 ██████████   |
  ███████████ |
   ███████████|
  ███████████ |
 ██████████   |
███████       |
--------------|

XOR
--------------|
█  ████       |
 █  ███████   |
  █  ████████ |
   █  ████████|
  █  ████████ |
 █  ███████   |
█  █████      |
--------------|
```