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
  (car (car (cdr (minimax position ply player t))))

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
	( play-again? player )
)

( defun play-again? ( player )
	"Ask the person if they would like to start over"
	( let 
        (  
        	input
        	choice	
        )

		( format t "Would you like to play again? [Y/N] " )


		;get user input...
		( setf input ( read-line ) )

		( with-input-from-string ( stream input )
			;...and set the FIRST number as ROW...
			( setf choice ( read stream NIL NIL ) )

		)

		;toupper...
		( setf choice (string-upcase choice ) )
		
		( cond

			;Restart game
			( 
				( or ( string= choice 'Y )
					 ( string= choice 'YES)
				)
				;ensure Black pass flag is T
				( othello-two-players player )
			)
		)
    )
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