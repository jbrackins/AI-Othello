#|
                  ***** MINIMAX.LSP *****

Generalized recursive minimax routine.

Author: Dr. John M. Weiss. Modifications made by Julian Brackins and
        Marcus Haberling
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (minimax position depth)
          where position is the position to be evaluated,
          and depth is the search depth (number of plys).

Returns:  (value path)
          where value is the backed-up value from evaluation of leaf nodes,
          and path is the path to the desired leaf node.

Functions called:

          (deepenough depth) -
              predicate that returns T if the current position has reached
              the desired search depth, NIL otherwise.

          (move-generator position) -
              generates successors to the position.

          (static position) -
              applies the static evaluation function to the position.

Modifications: 
        Adapted program to fit with our project, implemented alpha beta pruning.

|#


#|
  Name: deepenough
  Description: 
  The function to deepenought is used by Dr. Weiss minimax function
  to check if the program should stop going deeper. We don't have
  any special requirements of depth for our algorithim so it just stops
  at zero.

  Paramater:
    depth - current depth
|#
(defun deepenough (depth)
  (< depth 1)
)

(defun print-moves (list)
  (cond
    ( (null list) nil)
    (t
      (print-board (car list))
      (print-moves (cdr list))
    )
  )
)

#|
  Name: move-generator
  Description: 
  This function is used by Dr. Weiss's minimax function to generate possible
  moves. It workd by generating positions of moves using get-valid-moves
  and then turns them into board states using make-move-int.

  Paramater:
    position - current board state
    color - color of current player (black or white)
|#
(defun move-generator (position color)
  (loop for x in (get-valid-moves position color)
    collect (make-move-int position x color) 
  )
)

#|
  Name: static
  Description:
  static evaluation function for Dr. Weiss's minimax funtion.
  it calls our weighted-parity function to get its value

  Paramaters
    position - current board state
    color - player to calculate a value for.
|#
(defun static (position color)
    (weighted-parity position score-weights color)
)

#|
  Name: minimax
  Description:
  The mini-max function provided by Dr. Weiss modified to enable alpha 
  beta pruning. Alpha and beta are optional paramaters that should only
  be provided internally. If beta is ever found to be less then alpha
  the algorithim stops minimax on that level "pruning off" remaining states
  as a method to save computational time.

  Paramaters
    position - current board state
    depth - how many more levels to search down
    color - player to calculate a value for.
    max? : t - true for maximizing player, false for minimizing player
    alpha : -1000000 - the ever increasing alpha value
    beta  : 1000000 - the ever decreasing beta value.
|#
(defun minimax (position depth color &optional (max? t) (alpha -100000) (beta 100000) )

  ; if we have searched deep enough, or there are no successors,
  ; return position evaluation and nil for the path
  (if (or (deepenough depth) (null (move-generator position color)))
    (list (static position color) nil)

    ; otherwise, generate successors and run minimax recursively
    (let
      (
        ; generate list of sucessor positions
        (successors (move-generator position color))

        ; initialize current best path to nil
        (best-path nil)

        ; initialize current best score to negative infinity
        (best-score -1000000)

                ; other local variables
        succ-value
        succ-score
      )

      ; explore possible moves by looping through successor positions
      (dolist (successor successors)

        ; perform recursive DFS exploration of game tree
        (setq succ-value (minimax successor (1- depth) (other-color color) (not max?) alpha beta ))

        ; change sign every ply to reflect alternating selection
        ; of MAX/MIN player (maximum/minimum value)
        (setq succ-score (- (car succ-value)))

        ; update best value and path if a better move is found
        ; (note that path is being stored in reverse order)
        (when (> succ-score best-score)
          (setq best-score succ-score)
          (setq best-path (cons successor (cdr succ-value)))
        )

        (when (and max? (> best-score alpha))
          (setf alpha best-score)
        )
        (when (and (not max?) (< (- best-score) beta))
          (setf beta  (- best-score) )
        )

        (when (> alpha beta)
          (return)
        )
      )
      ; return (value path) list when done
      (list best-score best-path)
    )
  )
)
