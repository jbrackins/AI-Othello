#|
  file: heuristic-funcs.lsp

  Descritpion:
  This file contains all functions and constants used for the static evaluation
  function. Our program uses a variation of the coin-parity heuristic in which
  spaces are counted acording to weights.
|#

#|
  constants: score-weights
  description:
  A value map on the board that causes the weighted-count to
  prioritize different positions. All of the weights were assigned
  as powers of 2.The folling is a list of the weighted regionds and 
  the reasoning for those weights.

  Corners 32: 
    Corners had originally been 16, however in testing the
    ai gave them up to easily. 32 was a better number, 
    causing the ai to almost always take corners.
  Arreas around corners 01: 
    Since the areas around corners will give your opponents an
    opprotunity to take the corner they are the lowest score
    on the list.
  Outer Sides 16: 
    Outer sides seemed more important than the inner
    sides when playing because they are harder to flank. 
    When they were previously equivilent the ai didn't properly 
    deffend them which allowed the other player to build up a strong 
    side of the board more easily. 
  Inner Sides 8: 
    The inner sides are more important than the center board but the
    easiest to flank so we set those up as the weakest of the sides
    this makes the program build up a side from corner to the next.
  2nd Row/Col in 2: These spaces were set to 2  (the default weight) because
    they open up an opprotunity to take a side.
  3rd Row/Col in 4: These are usually safe moves so they are prioritized
    over the standard value of 2. They also open the opponent to less
    safe move.
  Starting Positions: These are given the standard value of 2 as to not
    over influence the game since they are both inconsequential and hard
    to control.              

  
|#
(defconstant score-weights
  '( 32 01 16 08 08 16 01 32
     01	01 02 02 02 02 01 01
     16 02 04 04 04 04 02 16
     08 02 04 02 02 04 02 08
     08 02 04 02 02 04 02 08
     16 02 04 04 04 04 02 16
     01 01 02 02 02 02 01 01
     32 01 16 08 08 16 01 32 )
)

#|
  name: score-weight other-color
  description:
  gives the opposite board color as the one provided
|#
(defun other-color (color)
  (cond
    ( (string= color 'B) 'W)
    ( (string= color 'W) 'B)
    ( T '- )
  )
)

#|
  name: weighted-count

  description:
  counts all the weights corresponding to board positions
  where a provided player has pieces. It runs through the list
  recursively if it encounters a color match it adds that positions
  weight to the sum

  paramaters:
    board - othello game board state
    weights - weights being passed in to compliment the board
    color - color of player to count for, black or white
|#
(defun weighted-count (board weights color)
  (cond
   ( (null board) 0)
   ( (equal color (car board))
     (+ (car weights) (weighted-count (cdr board) (cdr weights) color))
   )
   ( t (weighted-count (cdr board) (cdr weights) color))
  )
)

#|
  name: weighted-parity

  description:
  Based on the coin parity algorithim, it compares the weighted
  sums of a player and his oponents pieces on the board

  paramaters:
    board - othello game board state
    weights - weights being passed in to compliment the board
    max-color - color of player to count for, black or white
|#
(defun weighted-parity (board weights max-color)
  (let
    (
      ( max-count (weighted-count board weights max-color))
      ( min-count (weighted-count board weights (other-color max-color)))
    )
    (/ (* 100 (- max-count min-count) )(+ max-count min-count))
  )
)
