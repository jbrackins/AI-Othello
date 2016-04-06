#|
                    ***** GAME.LSP *****

Author:  J. Anthony Brackins, Marcus Haberling


Written Spring 2016 for CSC447/547 AI class.

Modifications: 

|#

#|--------------------------------------------------------------------------|#
#|                               Global Vars                                |#
#|--------------------------------------------------------------------------|#

( defparameter *blk_pass*  NIL )
( defparameter *wht_pass*  NIL )
( defparameter *game_board* 
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

( load 'print-funcs )


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

( defun counter ( atom lst )
	"Count all of a given atom"
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



( defun prompt-turn ( player ) 
	"Inform user it is their turn, validate the turn they input is acceptable"
	( let 
        ( 
            input
            row
            col
        )

		( print-board *game_board* )
		( format t "PLAYER: ~A~%" player )
		( format t "What is your move [row col]? " )

		;get user input...
		( setf input ( read-line ) )

        ( with-input-from-string ( stream input )
			;...and set the FIRST number as ROW...
			( setf row ( read stream NIL NIL ) )
			;...and set the SECOND number as COL.
			( setf col ( read stream NIL NIL ) )        
		)

        ( cond

            ;Place the disc if legal move
            ( (legal-move? row col ) 
                ( place-disc row col player )
            )

            ( T 
                ( format t "~%Illegal Move. Please Try Again.~%" )
                ( prompt-turn player )
            )
        )

        ( values )
	)
)

( defun place-disc ( row column player ) 
	"Place a Disc in the specified location, flip any pieces that are now flanked"
	( let 
        ( 
        	location
        )

        ;LOCATION is 
        ;( 8 x ( row - 1 ) + col ) - 1
        ; 8       = Board row length
        ; row - 1 = All rows after 1st must encorporate previous row lengths
        ; + col   = After multiplying by rows, add column offset
        ; - 1     = nth starts at 0, so subtract 1. 
        ( setf location 
        		( - ( + ( * ( - row 1 ) 8 ) column ) 1 ) 
    	)


		( cond
			;Place Black Disc
			( ( string= player "BLACK" ) 
				;place disc
				( setf (nth location *game_board*) "B" )
				;ensure Black pass flag is FALSE
				( setf *blk_pass* NIL )
			)

			;Place White Disc
			( ( string= player "WHITE" ) 
				;place disc
				(setf (nth location *game_board*) "W")
				;ensure White pass flag is FALSE
				( setf *wht_pass* NIL )
			)
		)

		( end-turn player )
    )
)

( defun end-turn ( player )
	"Switch turns to the other player"
	( cond
		;Switch to White Turn
		( ( string= player "BLACK" ) 
			( prompt-turn "WHITE" )
		)

		;Switch to Black Turn
		( ( string= player "WHITE" ) 
			( prompt-turn "BLACK" )
		)
	)
)

( defun legal-move? ( row column ) 
	"Check if a supplied place-disc move is legal"
    ( let 
        ( 
            location
            ( legal T ) ;Default this to true; let's try to see if it is false
        )

        ;LOCATION is 
        ;( 8 x ( row - 1 ) + col ) - 1
        ; 8       = Board row length
        ; row - 1 = All rows after 1st must encorporate previous row lengths
        ; + col   = After multiplying by rows, add column offset
        ; - 1     = nth starts at 0, so subtract 1. 
        ( setf location 
            ( - ( + ( * ( - row 1 ) 8 ) column ) 1 ) 
    	)



		( cond
			
		    ;THINGS TO CHECK:

		    ;IS THE SPOT ALREADY OCCUPIED
            ( 
                ( not 
                    ( string= ( nth location *game_board* ) "-") 
                ) 
                ( setf legal NIL )
            )
		)

        legal
	)
)

( defun pass-turn? ( board player ) 
	"Check if a player has to pass their turn"

)

( defun end-game? ( board player ) 
	"Check if both players just passed turns, thus ending the game"

)