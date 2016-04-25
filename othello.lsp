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


	( cond

		;Prompt for colour if none given
		( 
			( not ( null player ) )
			( setf player ( subseq player 0 1 ) )
		)
	)
	( othello-two-players player )
	;( othello-human-vs-ai player )
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


	( cond

		;Prompt for colour if none given
		( 
			( null player )
			( setf player ( prompt-player-colour ) )
		)
	)


	;just printing out the arg just to verify CLI is workin
	;(print player)
	( end-turn 'W board )
	( prompt-play-again? player "OTHELLO-TWO-PLAYERS" )
	( values )
)

( defun othello-human-vs-ai  ( &optional ( player nil ) 
							( board '( - - - - - - - -
				   					   - - - - - - - - 
				   					   - - - - - - - - 
				                       - - - W B - - - 
				                       - - - B W - - - 
				                       - - - - - - - - 
				                       - - - - - - - - 
				                       - - - - - - - - ) ) 
							) 
	"Starts up a game of othello where one player is human, other is ai"
	( let 
        (  
        	ai
        )

		( cond

			;Prompt for colour if none given
			( 
				( null player )
				( setf player ( prompt-player-colour ) )
			)
		)

		( cond

			;Set AI colour
			( ( string= player 'B ) 
				( setf ai 'W )
			)
			( ( string= player 'W ) 
				( setf ai 'B )
			)
		)


        
	)
)

( defun prompt-player-colour ()
	"Ask the person what colour they want to be"
	( let 
        (  
        	input
        	choice	
        )

		( format t "Would you like to play as " )
		( format t "~c[37;40mBLACK~c[0m or " #\ESC #\ESC  )
		( format t "~c[30;47mWHITE~c[0m ?~%"   #\ESC #\ESC )
		( format t "(~c[37;40mBLACK~c[0m moves first) > " #\ESC #\ESC  )


		;get user input...
		( setf input ( read-line ) )

		( with-input-from-string ( stream input )
			;...and set the FIRST number as ROW...
			( setf choice ( read stream NIL NIL ) )

		)

		;toupper...
		( setf choice (string-upcase choice ) )
		( setf choice ( subseq choice 0 1 ) )

		( cond

			;Check if proper user input
			( 
				( or ( string= choice 'B )
					 ( string= choice 'W)
				)
				;Correct, pass this as player choice
				choice
			)
			;Otherwise, re-prompt because they entered something wrong
			( T
				( prompt-player-colour )
			)
		)

    )
)

( defun prompt-play-again? ( player gametype )
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

				( cond

					;CHECK GAMETYPE
					( 
						( string= gametype "OTHELLO-TWO-PLAYERS" )
						( othello-two-players )
					)

				)
			)

			;End Game
			( 
				( or ( string= choice 'N )
					 ( string= choice 'NO)
				)

				( format t "See Ya~%" )
			)



			;else: Reprompt
			( T
				( prompt-play-again? player gametype )
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