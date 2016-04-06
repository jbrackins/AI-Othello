#|
                    ***** GAME.LSP *****

Author:  J. Anthony Brackins, Marcus Haberling


Written Spring 2016 for CSC447/547 AI class.

Modifications: 

|#

( defun count-discs ( board ) 
	"Return Player Scores AKA how many pieces they currently have"
	( let 
        (  
        	( blk 0 ) ;BLACK pieces
        	( wht 0 ) ;WHITE pieces
        )

        ( setf blk ( counter 'B board ) )
        ( setf wht ( counter 'W board ) )

        ( list blk wht )
    )
)

( defun declare-winner ( scores ) 
	"Announce Winner of the game based on count list"
	( let 
        (  
        	( blk_score ( car  scores ) ) ;BLACK score
        	( wht_score ( cadr scores ) ) ;WHITE score
        	winner 					     ;WINNING COLOUR
        )

		( cond
	
			;Black's score is higher
			( ( > blk_score wht_score ) 
				( setf winner "BLACK WINS" )
			)

			;White's score is higher
			( ( > wht_score blk_score ) 
				( setf winner "WHITE WINS" )
			)

			;ELSE, Tie Game
			( t 
				( setf winner "TIE GAME" )
			) 

		)

		;Print out the score
		( format t "BLACK  SCORE: ~A DISCS~%" blk_score )
		( format t "WHITE  SCORE: ~A DISCS~%" wht_score )
		( format t "GAME RESULTS: ~A~%" winner )
		( values )
    )
)

( defun counter ( atom lst )
	( cond
	
		;End of the list OR list was empty all along.
		( ( null lst ) 
			0 
		)

		;IF the CAR is the atom you're looking for,
		;Increment count and recurse on the cdr
		( ( equal atom ( car lst ) ) 
			( + 1 ( counter atom ( cdr lst ) ) ) 
		)

		;ELSE, recurse on the cdr, no incrementation
		( t 
			( counter atom ( cdr lst ) ) 
		) 

	)
)

( defun place-disc ( row column player ) 
	"Place a Disc in the specified location, flip any pieces that are now flanked"

)

( defun legal-move? ( row column ) 
	"Check if a supplied place-disc move is legal"

)

( defun pass-turn? ( board player ) 
	"Check if a player has to pass their turn"

)

( defun end-game? ( board player ) 
	"Check if both players just passed turns, thus ending the game"

)