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
		( format t "~%GAME OVER. NO REMAINING MOVES~%" )
		( print-player 'B )
		( format t "  SCORE: ~A DISCS~%" blk_score )
		( print-player 'W )
		( format t "  SCORE: ~A DISCS~%" wht_score )
		( format t "GAME RESULTS: ~A~%" winner )
		( values )
    )
)



( defun prompt-turn ( player board ) 
	"Inform user it is their turn, validate the turn they input is acceptable"
	( let 
        ( 
            input
            row
            col
            temp-board
        )

		(setf temp-board (copy-list board))

		( print-board temp-board )


		( cond
			;end game?
			( ( end-game? board )

				( declare-winner  ( count-discs board )   )			

			)

			;pass player turn?
			( ( pass-turn? player  board ) 
				( cond
					;Place Black Disc
					( ( string= player 'B ) 
						;ensure Black pass flag is T
						( setf *blk_pass* T )
					)

					;Place White Disc
					( ( string= player 'W ) 
						;ensure White pass flag is T
						( setf *wht_pass* T )
					)
				)

				( format t "NO MOVES AVAILABLE FOR: ")
				( print-player player )
				( format t "~%")

				( end-turn player board )
			)

			;otherwise, promt the user and let them play
			( t

				( format t "PLAYER: ")
				( print-player player )
				( format t "~%")
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
		            ( (legal-move? player  temp-board row col )
		            	( place-disc player  board row col ) 
	            	 	( flip-at player board row col ) 
		                ( end-turn player board )
		            )

		            ;else
		            ( T 
		                ( format t "~%Illegal Move. Please Try Again.~%" )
		                ( prompt-turn player board )
		            )
		        )
			)
		)
        ( values )
	)
)

( defun place-disc ( player board row column ) 
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
			( ( string= player 'B ) 
				;place disc
				( setf (nth location board) "B" )
				;ensure Black pass flag is FALSE
				( setf *blk_pass* NIL )
			)

			;Place White Disc
			( ( string= player 'W ) 
				;place disc
				(setf (nth location board) "W")
				;ensure White pass flag is FALSE
				( setf *wht_pass* NIL )
			)
		)

		;( end-turn player )
    )
)

( defun end-turn ( player board )
	"Switch turns to the other player"
	( cond
		;Switch to White Turn
		( ( string= player 'B ) 
			( prompt-turn 'W board )
		)

		;Switch to Black Turn
		( ( string= player 'W ) 
			( prompt-turn 'B board )
		)
	)
)


( defun count-legal-moves ( player board )
	"Check if a player has any legal moves"
    ( let 
        ( 
            row
            col
            location
            temp-board
            ( moves 0 )
        )

    
		(setf temp-board (copy-list board))

		;incredibly bad version:
		( loop for row from 1 to 8 do 
			( loop for col from 1 to 8 do 

			  	( setf location 
		            ( - ( + ( * ( - row 1 ) 8 ) col ) 1 ) 
		    	)
				;(format t "row: ~A col: ~A ~%" row col)
				( cond 
					( ( string= "-" ( nth location temp-board ) )
						
						( cond 
							( ( legal-move? player temp-board row col ) 
								( incf moves )
								(setf temp-board (copy-list board))
							)
						)
					)
				)
			)
		)

		;(format t "~A Moves Available ~%" moves)
		moves

	)
)

( defun legal-move? ( player board row column ) 
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
                    ( < row 1 )    ( > row 8 ) 
                    ( < column 1 ) ( > column 8 ) 
                ) 
                ( setf legal NIL )
            )
		    ;IS THE SPOT ALREADY OCCUPIED
            ( 
                ( not 
                    ( string= ( nth location board ) "-") 
                ) 
                ( setf legal NIL )
            )
            ( T
    			;Try to perform flips, if none happen, then illegal move
				( setf flips ( flip-at player board row column ) )
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

( defun pass-turn? ( player board ) 
	"Check if a player has to pass their turn"

	( cond
		;Are there any legal moves? if not.... pass turn
		( ( < ( count-legal-moves player board ) 1  )
				;( format t "No available moves for ~A...~%" player )
			T
		)
		;otherwise, don't pass turn.
		( T
			NIL
		)
	)
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


( defun keep-moving? ( curr direction )
	( let 
		(
			location
			( left-edge   '( 00 08 16 24 32 40 48 56 ) )
			( right-edge  '( 07 15 23 31 39 47 55 63 ) )
			( top-edge    '( 00 01 02 03 04 05 06 07 ) )
			( bottom-edge '( 56 57 58 59 60 61 62 63 ) )
			( moving t )
		)

		;(format t "curr: ~A dir: ~A ~%" curr direction)
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

		;(print moving)
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

        ;set location
        ( setf location 
            ( - ( + ( * ( - row 1 ) 8 ) column ) 1 ) 
    	)

        ;determine what colour the player pieces are,
        ;and what colour the opponent pieces are.
		( cond
			( ( string= player 'B ) 
				(setf player-piece "B")
				(setf opponent-piece "W")
			)

			( ( string= player 'W ) 
				(setf player-piece "W")
				(setf opponent-piece "B")
			)
		)

		;set curr to the piece location	
		(setf curr location)
		
		;For each direction in the list of directions (8 dirs)
		( loop for dir in directions do 

			;reset whether you've found a bracket
			( setf found-bracket nil )
			
			;( format t "~A ~%" dir )
			
			;increment curr in the direction you want to move
			(setf curr (+ curr dir))
			( cond
	
				;Short circuit to handle -1 or 64 out of bounds
				( (or ( > curr 63) ( < curr 0 )) 
					nil
					;(print "SHIT")
				)

				;Found one of your opponent's pieces directly
				;adjacent to the piece you wanna place, 
				;start looking for a bracket.
				( ( string= opponent-piece (nth curr board)  ) 
					(setf flip-list (append  flip-list (list curr)) )
					;( format t "FOUND AT: ~A~%" curr )
					;( format t "WHAT ~A~%" flip-list )

					;keep-moving? function will check if you 
					;have hit out of bounds.
					( loop while ( keep-moving? curr dir ) do
						(setf curr ( + curr dir ) )


						( cond

							;Found another one of your opponent's pieces,
							;keep going until you hit a bracket, hit space, 
							;or step out of bounds
							( ( string= opponent-piece (nth curr board)  ) 
								;found another of the same colour, KEEP MOVING
								;( format t "FOUND ANother AT: ~A~%" curr )
								;push location onto list to reverse
								(setf flip-list (append  flip-list (list curr)) )
							)

							;If you find a bracket piece:
							( ( string= player-piece (nth curr board)  )
								( cond 
									;additional flag to prevent finding a "double bracket"
									( ( not found-bracket )
										;( format t "FOUND BRAKCET AT: ~A~%" curr )
										;( format t "WHAT ~A~%" flip-list )
										( cond 

											;if the brackit is bad, don't do flips
											( ( member -1000 flip-list )
												;( format t "Bad Bracket: No FLips~%" )


											)

											;otherwise, process the flip list
											( t
												( dolist (q flip-list )
													;( format t "FLIPPING: ~A~%" q )
													( cond

														;Flip BLACK to White
														( ( string=  (nth q  board )  "B" ) 
															(setf  (nth q  board ) "W")
															( incf num-flipped )

														)

														;Flip White to Black
														( ( string=  (nth q  board )  "W" ) 
															(setf  (nth q  board ) "B")
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
								;Hit a gap, anything beyond this is certainly
								;not a legal bracket
								(setf flip-list (append  flip-list (list -1000)) )
								;( format t "Not Good~%")
							)
						)
					)
					;clear out the flip list
					(setf flip-list NIL)
				)
			)
			;reset your curr iterator back to the piece location
			(setf curr location)
		)
		;return how many tiles were flipped, as this will be used in min-max
		;( format t "~A tiles flipped ~%" num-flipped)
		num-flipped
	)


)

( defun end-game? ( board ) 
	"Check if both players just passed turns, thus ending the game"
	( cond
		;if both players skip, then game is over
		(
			( and 
				( not ( null *blk_pass* ) )
				( not ( null *wht_pass* ) )
		  	)
		  	T
		)

		;otherwise, game isn't over
		( t
			nil
		)
	)
)