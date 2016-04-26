#|
  constants: score-weights
  description:
  A value map on the board that causes the weighted-count to
  prioritize different positions. This weight map prioritize corners
  then sides. It also avoids positions that give the opponent the
  opprotunity at corners and sides.
|#
(defconstant score-weights
  '( 32 01 16 08 08 16 01 32
     01	01 02 02 02 02 01 01
     16 02 04 04 04 04 02 16
     08 02 04 02 02 04 02 08
     08 02 04 02 02 04 02 08
     16 02 04 04 04 04 02 16
     01 01 02 02 02 02 02 02
     32 01 16 08 08 16 02 32 )
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
