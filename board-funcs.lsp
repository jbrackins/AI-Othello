#|
                  ***** BOARD-FUNCS.LSP *****

  description:
  This file contains the functions that check if
  moves are valid and make moves on the board itself. the two
  main external functions are get-valid-moves that returns
  the moves a player can make and make-move-int that returns
  an new board state after making a move.

  Author:  J. Anthony Brackins, Marcus Haberling


  Written Spring 2016 for CSC447/547 AI class.

|#


#|
  name: direction constants
 
  description:
  The Direction Constants are a more readable way to
  shift board position in one of the 8 directions
  that need to be checked in relation to move making in othello
|#
(defconstant NORTH     -8)
(defconstant NORTHEAST -7)
(defconstant EAST       1)
(defconstant SOUTHEAST  9)
(defconstant SOUTH      8)
(defconstant SOUTHWEST  7)
(defconstant WEST      -1)
(defconstant NORTHWEST -9)

(defun new-board ()
  "Creates an empty othello board"
  '( - - - - - - - -
     - - - - - - - -
     - - - - - - - -
     - - - B W - - -
     - - - W B - - -
     - - - - - - - -
     - - - - - - - - 
     - - - - - - - -) 
)

#|
  Name: make-safe-movedirection
  Description:
  converts an row col pair into a value
  0-63 on the board
  Paramaters:
    pos-x - the row
    pos-y - the column
|#
(defun make-pos (pos-x pos-y)
  (+ (- pos-x 1) (* (- pos-y 1) 8))
)


#|
  Name: On-Board
  Description:
  Checks if a move from a provided position is still on the board
  checking mathematically if that move is possible from the starting
  position. Eg, if the move is right then the position we are
  moving from cant be in the right most row.

  Paramaters:
    pos - current position on the board 0-63
    move - place to move on the board from the position
           usually passed in as one of the direction 
           constants.
|#
(defun on-board (pos move)
  "Checkis if a move from the provided position is on the board"
  (cond
    ((= move NORTH) (> pos 7))
    ((= move NORTHEAST) (and (> pos 7) (not (= (mod pos 8) 7))))
    ((= move EAST) (not (= (mod pos 8) 7)))
    ((= move SOUTHEAST) (and (< pos 56) (not (= (mod pos 8) 7))))
    ((= move SOUTH) (< pos 56))
    ((= move SOUTHWEST) (and (< pos 56) (not (= (mod pos 8) 0))))
    ((= move WEST) ( and (not (= (mod pos 8) 0)) ( > pos 1 ) )  )
    ((= move NORTHWEST) (and (> pos 7) (not (= (mod pos 8) 0))))
    (t nil)
  )
)

#|
  Name: valid-move-direction
  Description:
  An inner function to be called by valid-move to check if the move
  acomplishes a flank in a provided direction. This function
  recursively calls itself on moves in a provided direction.
  - If you move off the board, false is returned
  - if you encounter your color, there is no flank to be made,
    return null
  - If you encounter the enemy color, you may be able to flank, call
    recursively with the good variable set to true instead of false.
  - If you encounter a space check the good variable to see if you
    have seen a an enemy piece yet. If you have return true else
    return false.
  

  Paramaters:
    board - current board state
    pos - current position on the board 0-63
    move - place to move on the board from the position
           usually passed in as one of the direction 
           constants.
    color - color black or white of current player
    good - have we seen an opponent piece yet?
|#
(defun valid-move-direction (board pos move color good)
  "Checks if moving in a specified direction validates a move position"
  (let ( (newPos (+ pos move)) )
    (cond
      ( (not (on-board pos move)) nil)
      ;Modified these to string= because they seem to work 
      ;more consistently.... JB
      ( (string= color (nth newPos board)) good)
      ( (string= '- (nth newPos board)) nil)
      ( t (valid-move-direction board newPos move color t) )
    )
  )    
)

#|
  Name: valid-move
  Description:
  valid-move checks if a provided move can be made.
  It assumes the move is on the board. It checks if the
  space to move is empty. It then calls valid-move-direction
  in all directions to see if there is any position that you
  can flak from that position. If there is one it returns
  true else false.

  Paramaters:
    board - current board state
    pos - current position on the board 0-63
    color - color black or white of current player
|#
(defun valid-move (board pos color)
  (if (equal (nth pos board) '-)
    (or
       (valid-move-direction board pos NORTH     color nil)
       (valid-move-direction board pos NORTHEAST color nil)
       (valid-move-direction board pos EAST      color nil)
       (valid-move-direction board pos SOUTHEAST color nil)
       (valid-move-direction board pos SOUTH     color nil)
       (valid-move-direction board pos SOUTHWEST color nil)
       (valid-move-direction board pos WEST      color nil)
       (valid-move-direction board pos NORTHWEST color nil)
    )
    nil
  )
)

#|
  Name: get-valid-moves
  Description:
  returns a list of moves a player of the provided color could make
  by calling valid-move on every board posistion, if the move is valid it
  adds that move to the list.

  Paramaters:
    board - current board state
    color - color black or white of current player
|#
(defun get-valid-moves (board color)
  (let ( (return-list nil) )
    (loop for x from 0 to 63 do
      (if (valid-move board x color) (setq return-list (cons x return-list)))
    )

    return-list
  )
)

#|
  Name: make-move-direction
  Description:
  Nearly identical logic to the valid-move-direction. instead of just returning
  true if there is a flank, this function flips all the pieces its encounters
  if not it returns nil instead of a new board.  
  by calling valid-move on every board posistion, if the move is valid it
  adds that move to the list.

  Paramaters:
    board - current board state
    pos - current position on the board 0-63
    move - place to move on the board from the position
           usually passed in as one of the direction 
           constants.
    color - color black or white of current player
    good - have we seen an opponent piece yet?
|#
(defun make-move-direction (board pos move color good)
  (let ( (new-pos (+ pos move)) )
    (cond
      ( (not (on-board pos move)) nil) 
      ( (equal color (nth new-pos board)) good)
      ( (equal '- (nth new-pos board)) nil)
      ( (make-move-direction board new-pos move color t)
	(setf (nth new-pos board) color))
      ( t nil)
    )
  )
)

#|
  Name: make-move-direction
  Description:
  Makes a new board state by flipping the all flanked tiles in 
  all directions using the make-move-direction function
  then returns the new state. This function doesnt check if the
  move was valid in the first place.

  Paramaters:
    board - current board state
    pos - current position on the board 0-63
    color - color black or white of current player
|#
(defun make-move-int (board pos color)
  (let ( (new-board (copy-list board)) )
    (setf (nth pos new-board) color)
    (make-move-direction new-board pos NORTH     color nil)
    (make-move-direction new-board pos NORTHEAST color nil)
    (make-move-direction new-board pos EAST      color nil)
    (make-move-direction new-board pos SOUTHEAST color nil)
    (make-move-direction new-board pos SOUTH     color nil)
    (make-move-direction new-board pos SOUTHWEST color nil)
    (make-move-direction new-board pos WEST      color nil)
    (make-move-direction new-board pos NORTHWEST color nil)
    new-board
  )
  )

#|
  Name: make-safe-movedirection
  Description:
  A testing and development function to check if a provided
  move is safe and make it if it is, returning the new board state.

  Paramaters:
    board - current board state
    pos - current position on the board 0-63
    color - color black or white of current player
|#
(defun make-safe-move (board pos color)
  (if (valid-move board pos color)
    (make-move-int board pos color)
    board
  )
)
