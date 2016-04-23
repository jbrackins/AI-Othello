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

(defun make-pos (pos-x pos-y)
  (+ (- pos-x 1) (* (- pos-y 1) 8))
)

(defun on-board (pos move)
  "Checkis if a move from the provided position is on the board"
  (cond
    ((= move NORTH) (> pos 7))
    ((= move NORTHEAST) (and (> pos 7) (not (= (mod pos 8) 7))))
    ((= move EAST) (not (= (mod pos 8) 1)))
    ((= move SOUTHEAST) (and (< pos 56) (not (= (mod pos 8) 7))))
    ((= move SOUTH) (< pos 56))
    ((= move SOUTHWEST) (and (< pos 56) (not (= (mod pos 8) 0))))
    ((= move WEST) (not (= (mod pos 8) 0)))
    ((= move NORTHWEST) (and (> pos 7) (not (= (mod pos 8) 0))))
    (t nil)
  )
)

(defun valid-move-direction (board pos move color good)
  "Checks if moving in a specified direction validates a move position"
  (let ( (newPos (+ pos move)) )
    (cond
      ( (not (on-board pos move)) nil)
      ( (equal color (nth newPos board)) good)
      ( (equal '- (nth newPos board)) nil)
      ( t (valid-move-direction board newPos move color t) )
    )
  )    
)

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

(defun get-valid-moves (board color)
  (let ( (return-list nil) )
    (loop for x from 0 to 63 do
      (if (valid-move board x color) (setq return-list (cons x return-list)))
    )

    return-list
  )
)

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

(defun make-safe-move (board pos color)
  (if (valid-move board pos color)
    (make-move-int board pos color)
    board
  )
)
