#|
                    ***** OTHELLO.LSP *****

Author:  J. Anthony Brackins, Marcus Haberling


Written Spring 2016 for CSC447/547 AI class.

Modifications: 

|#

#|--------------------------------------------------------------------------|#
#|                               Files Loaded                               |#
#|--------------------------------------------------------------------------|#

( load 'print-funcs )
( load 'game        )
( load 'minimax     )

#|--------------------------------------------------------------------------|#
#|                               SAMPLE BOARDS                              |#
#|--------------------------------------------------------------------------|#
;The following are a few different sample boards one can read in to verify
;a few different strategies for the game. Many of these board states were 
;sourced from http://www.samsoft.org.uk/reversi/strategy.htm and 
;exemplify certain strategies or notewothy nuances in othello strategy.

( defparameter *board-start* 
	                        '( - - - - - - - -
		   					   - - - - - - - - 
		   					   - - - - - - - - 
		                       - - - W B - - - 
		                       - - - B W - - - 
		                       - - - - - - - - 
		                       - - - - - - - - 
		                       - - - - - - - - 
   		                     ) 
)

;A general fallacy to othello is that ending up with the most discs is the 
;only major goal. However, this game illustrates that having the most pieces
;does not always translate to victory. In this scenario, white is forced to 
;skip every turn, and black will manage to flip over enough tiles to gain
;victory.
( defparameter *board-too-much-too-soon* 
	                        '( - W W W W W W -
		   					   W W W W W W W W 
		   					   W W W W W W W W 
		                       W W W B W W W W  
		                       W W W W W W W W 
		                       W W W W W W W W  
		                       W W W W W W W W 
		                       - W W W W W W -
   		                     ) 
)


#|--------------------------------------------------------------------------|#
#|                         Tournament Functions                             |#
#|--------------------------------------------------------------------------|#

( defun make-move ( position player ply ) 
  "function to allow Othello programs interact in computer tournament"
  (setf _ALPHA_ -1000000)
  (setf _BETA_   1000000)
  (car (car (cdr (minimax position ply player t))))

)

( defun c-v-c ( board color )
  (cond 
    ( (setf board (make-move board color 4))
      (print-board board )
      (c-v-c board (other-color color)) )
  )
)


( defun othello ( &optional ( player nil ) ) 
	"Starts up a game of othello."

	;just printing out the arg just to verify CLI is workin
	(print player)
)

( defun othello-two-players ( &optional ( player nil ) 
							( board '( - - - - - - - -
				   					   - - - - - - - - 
				   					   - - - - - - - - 
				                       - - - W B - - - 
				                       - - - B W - - - 
				                       - - - - - - - - 
				                       - - - - - - - - 
				                       - - - - - - - - ) ) 
							) 
	"Starts up a game of othello where both players are human."

	;just printing out the arg just to verify CLI is workin
	;(print player)
	( end-turn 'W board )
)

( defun othello-init ()
	"Function called once prior to tournament for initialization purposes"

)

#|--------------------------------------------------------------------------|#
#|                              MAIN FUNCTION                               |#
#|--------------------------------------------------------------------------|#

; This function handles the case when this script is run as an argument
; to the interpreter
( defun main ()
    "Automatically calls the othello function when othello.lsp script is run."
	( when ( = ( length *args* ) 1 )
	    ( othello ( car *args* ) )
    )
)

( main )
