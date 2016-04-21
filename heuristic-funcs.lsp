(defconstant score-weights
  '( 16 08 08 08 08 08 08 16
     08	02 02 02 02 02 02 08
     08 02 04 04 04 04 02 08
     08 02 04 01 01 04 02 08
     08 02 04 01 01 04 02 08
     08 02 04 04 04 04 02 08
     08 02 02 02 02 02 02 08
     16 08 08 08 08 08 08 16 )
)

(defun other-color (color)
  (cond
    ( (equal color 'B) 'W)
    ( (equal color 'W) 'B)
    ( T '- )
  )
)

(defun weighted-count (board weights color)
  (cond
   ( (null board) 0)
   ( (equal color (car board))
     (+ (car weights) (weighted-count (cdr board) (cdr weights) color))
   )
   ( t (weighted-count (cdr board) (cdr weights) color))
  )
)

(defun weighted-parity (board weights max-color)
  (let
    (
      ( max-count (weighted-count board weights max-color))
      ( min-count (weighted-count board weights (other-color max-color)))
    )
    (/ (* 100 (- max-count min-count) )(+ max-count min-count))
  )
)


(defun count-color (board color)
  (cond
   ( (NULL board) 0 )
   ( (equal color (car board)) (+ 1 (count-color (cdr board))))
   (t (count-color (cdr board)))
  )
)

(defun coin-parity (board max-color)
  (let
    (
      ( max-count (count-color board max-color))
      ( min-count (count-color board (other-color max-color)))
    )
    (/ (* 100 (- max-count min-count) )(+ max-count min-count))
  )   
)


(defun corners-captured (board max-color)
  (let ( max-corner min-corner )
    

  )   
)
