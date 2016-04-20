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

        ( setf blk ( counter "B" board ) )
        ( setf wht ( counter "W" board ) )

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
		( ( string= atom ( car lst ) ) 
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
            ( (legal-move? player row col )
            	( place-disc player row col ) 
                ( end-turn player )
            )

            ;else
            ( T 
                ( format t "~%Illegal Move. Please Try Again.~%" )
                ( prompt-turn player )
            )
        )

        ( values )
	)
)

( defun place-disc ( player row column ) 
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

		;( flip-at player *game_board* row column )
		;( end-turn player )
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

( defun legal-move? ( player row column ) 
	"Check if a supplied place-disc move is legal"
    ( let 
        ( 
            location
            ( legal T ) ;Default this to true; let's try to see if it is false
            flips
        )

        ;LOCATION is 
        ;( 8 x ( row - 1 ) + col ) - 1
        ; 8       = Board row length
        ; row - 1 = All rows after 1st must encorporate previous row lengths
        ; + col   = After multiplying by rows, add column offset
        ; - 1     = nth starts at 0, so subtract 1. 
        ( cond 
        	( ( and 
        			( not (null row) )
        			( not (null column) )

    			)

		        ( setf location 
		            ( - ( + ( * ( - row 1 ) 8 ) column ) 1 ) 
		    	)

    		)

    	)




		( cond
			
		    ;THINGS TO CHECK:
		    ;did you not enter anything
            ( 
                ( or 
                    ( null row ) 
                    ( null column ) 
                ) 
                ( setf legal NIL )
            )
		    ;Row is out of bounds
            ( 
                ( or 
                    ( < row 1 ) 
                    ( > row 8 ) 
                    ( < column 1 ) 
                    ( > column 8 ) 
                ) 
                ( setf legal NIL )
            )
		    ;IS THE SPOT ALREADY OCCUPIED
            ( 
                ( not 
                    ( string= ( nth location *game_board* ) "-") 
                ) 
                ( setf legal NIL )
            )
            ( T
    			;Try to perform flips, if none happen, then illegal move
				( setf flips ( flip-at player *game_board* row column ) )
				( cond
		            ( 
		                ( < flips 1 ) 
		                ( setf legal NIL )
		            )
				)

        	)
		)



        legal
	)
)

( defun pass-turn? ( board player ) 
	"Check if a player has to pass their turn"

)


( defun check-flip-dirs ( row column directions )  
	( let 
		(
			location
			( left-edge  '( 00 08 16 24 32 40 48 56 ) )
			( right-edge '( 07 15 23 31 39 47 55 63 ) )
		)

		( setf location 
			( - ( + ( * ( - row 1 ) 8 ) column ) 1 ) 
		)



		( cond
			
			;Check if it's in the left edge
		    ( 
		        ( member location left-edge )
		        ;remove -9, -1, 7
		        ( setf directions ( remove -9 directions ))
		        ( setf directions ( remove -1 directions ))
		        ( setf directions ( remove  7 directions ))

		    )

			;Check if it's in the right edge
		    ( 
		        ( member location right-edge )
		        ;remove -7, 1, 9
		        ( setf directions ( remove  9 directions ))
		        ( setf directions ( remove  1 directions ))
		        ( setf directions ( remove -7 directions ))

		    )
		)
		directions


	)




)


( defun keep-moving ( curr direction )
	( let 
		(
			location
			( left-edge   '( 00 08 16 24 32 40 48 56 ) )
			( right-edge  '( 07 15 23 31 39 47 55 63 ) )
			( top-edge    '( 00 01 02 03 04 05 06 07 ) )
			( bottom-edge '( 56 57 58 59 60 61 62 63 ) )
			( moving t )
		)

		(format t "curr: ~A dir: ~A ~%" curr direction)
		( cond

			(
				( or 
					( < curr 0  )
					( > curr 64 )
				 )
				( setf moving nil )

			)
			;on the left edge AND you're moving 
			;-9 OR -1 OR 7
			(
				( and ( member curr left-edge )
				( or ( eq direction -9 ) 
					 ( eq direction -1 ) 
					 ( eq direction  7 ) 
				) )
				
				( setf moving nil )
			)

			;on the right edge AND you're moving 
			;9 OR 1 OR -7
			(
				( and ( member curr right-edge )
				( or ( eq direction 9 ) 
					 ( eq direction 1 ) 
					 ( eq direction -7 ) 
				) )
				
				( setf moving nil )
			)

			;on the top edge AND you're moving 
			;-9 OR -8 OR -7
			(
				( and ( member curr top-edge )
				( or ( eq direction -9 ) 
					 ( eq direction -8 ) 
					 ( eq direction -7 ) 
				) )
			
				( setf moving nil )
			)

			;on the bottom edge AND you're moving 
			;9 OR 8 OR 7
			(
				( and ( member curr bottom-edge )
				( or ( eq direction 9 ) 
					 ( eq direction 8 ) 
					 ( eq direction 7 ) 
				) )
				
				( setf moving nil )
			)
		)

		(print moving)
		moving
	)

)

( defun flip-at ( player board row column )
	"Check for flips at a given location"
    ( let 
        ( 
            ( bracket '(row column) )

            player-piece
            opponent-piece
            location
            curr
			flip-list            
			;  -09 -08 -07
			;  -01  00  01
			;   07  08  09	
            (  directions '(-9 -8 -7 -1 1 7 8 9) )
            (num-flipped 0)
            ( found-bracket nil )
        )

        ;update directions list based on if you're on left or right
        ( setf directions ( check-flip-dirs row column directions ) )
        (setf flip-list '())

        ( setf location 
            ( - ( + ( * ( - row 1 ) 8 ) column ) 1 ) 
    	)



		( cond
	
			( ( string= player "BLACK" ) 
				(setf player-piece "B")
				(setf opponent-piece "W")

			)

			( ( string= player "WHITE" ) 
				(setf player-piece "W")
				(setf opponent-piece "B")
			)

		)
	
		;(print (list-length board))
		(setf curr location)
		( loop for dir in directions do 

			;reset whether you've found a bracket
			( setf found-bracket nil )
			( format t "~A ~%" dir )
			
			(setf curr (+ curr dir))
			( cond
	
				( (or ( > curr 63) ( < curr 0 )) 

					(print "SHIT")
				)
				( ( string= opponent-piece (nth curr board)  ) 
					( format t "FOUND AT: ~A~%" curr )
					(setf flip-list (append  flip-list (list curr)) )
					( format t "WHAT ~A~%" flip-list )

					;(setf curr (+ curr dir))
					( loop while ( keep-moving curr dir ) do
						(setf curr ( + curr dir ) )


						( cond
							( ( string= opponent-piece (nth curr board)  ) 
								;found another of the same colour, KEEP MOVING
								( format t "FOUND ANother AT: ~A~%" curr )
								;push location onto list to reverse
								(setf flip-list (append  flip-list (list curr)) )


							)
							( ( string= player-piece (nth curr board)  )
								;found BRACKET PIECE 
								( cond 
									( ( not found-bracket )


										( format t "FOUND BRAKCET AT: ~A~%" curr )
										( format t "WHAT ~A~%" flip-list )

										;( reversi location curr dir board )
										( cond 
											;bad bracket, don't do flips
											( ( member -1000 flip-list )
												( format t "Bad Bracket: No FLips~%" )


											)

											;otherwise, process the flip list
											( t

												( dolist (q flip-list )
													( format t "FLIPPIN: ~A~%" q )
													( cond

														( ( string=  (nth q  *game_board* )  "B" ) 
															( format t "BLAH: ~A~%" q )
															(setf  (nth q  *game_board* ) "W")
															( incf num-flipped )

														)

														( ( string=  (nth q  *game_board* )  "W" ) 
															(setf  (nth q  *game_board* ) "B")
															( incf num-flipped )

														)

													)

												) 


											)



										)

										;Empty flip list and set curr iterator very high
										;and set that a bracket has been found
										(setf flip-list NIL)
										(setf curr 100)
										(setf found-bracket T)
									)

								)

								
							)
							( t
								;(setf flip-list NIL)
								(setf flip-list (append  flip-list (list -1000)) )
								( format t "Not Good~%")

							)
						)


						;(print "hi")
					)
					(setf flip-list NIL)

				)




			)	

			(setf curr location)

		)
		( format t "~A tiles flipped ~%" num-flipped)
		num-flipped
	)


)

( defun end-game? ( board player ) 
	"Check if both players just passed turns, thus ending the game"

)