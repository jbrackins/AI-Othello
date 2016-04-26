#|
                    ***** GAME.LSP *****

Author:  J. Anthony Brackins, Marcus Haberling

Written Spring 2016 for CSC447/547 AI class.

Modifications: 

|#

#|
  Name: find-location
  Description:
  looks at the board states before and after placing 
  a disc from the minimax function to determine the 
  location of the board in which a piece was placed. 
  This can later be converted to row & column values.
  Paramaters:
    old-board - board state before performing minimax
    new-board - board state after  performing minimax
|#
(defun find-location ( old-board new-board )
  "Determine location of piece placed"
  (let
	  (
	   ; generate list of sucessor positions
	    ( old ( flatten-board old-board ) )
	    ( new ( flatten-board new-board ) )
	    location   
	    ( i 0 )
	  )

	  ;Find location that is different between boards
	  ( loop while ( < i 64 ) do     
	    (cond
	      ( ( not (string= ( nth i old ) ( nth i new )) )
	            (setf location i)
	          ( setf i 100 )
	      )
	    )
	    (incf i)
	  )
	  location
  )
)

#|
  Name: flatten-board
  Description:
  Converts every 'B and 'W on the board to an 'X for 
  purposes of determining where a new piece has been 
  placed on a board.
  Paramaters:
    board - game board
|#
(defun flatten-board ( board )
  "flatten every piece to the same symbol"
  ( map 'list
    #'(lambda (x) 
      (if ( or (string= x 'B) (string= x 'W) ) 
        ;TRUE
        (setf x 'X) 
        ;FALSE
        (setf x '-))
      )
  board )
)

#|
  Name: loc-to-row-col
  Description:
  Converts a location value (0-63) to its 
  corresponding row-col pair.
  Parameters:
    location: location value on the board.
|#
( defun loc-to-row-col ( location )
  "Convert location in array to row/col list"
  ( let 
    (
      ( row 0 )
      ( col 0 )
      ( i 8 )
      ( j 1 )
    )

    ( loop while ( < j 9 ) do
      ( cond 
        ( ( < location i )
          ( setf row j )
          ;break out of loop
          ( setf j 100 )
        )
      )
      ;increment i by 8 (go to next row)
      ( setf i ( + i 8 ) )
      ;increment j by 1 (next row value)
      ( setf j ( + j 1 ) )
    )

    ;column is ( location mod 8 ) + 1
    ( setf col ( + ( mod location 8 ) 1 ) )

    ;return (row col) list
    ( list row col )
  )
)

#|
  Name: count-discs
  Description:
  Count up the number of Black and White discs on the board.
  Paramaters:
    board - game board
|#
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

#|
  Name: counter
  Description:
  Counter function for recursively tallying the number 
  of pieces for a given piece color. 
  Paramaters:
    atom - a given location in a list
    lst  - a given list (in this case its the board)
|#
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

#|
  Name: declare-winner
  Description:
  Declare who has won the game of othello
  Paramaters:
    scores - ( Black_Score White_Score )
|#
( defun declare-winner ( scores ) 
  "Announce Winner of the game based on count list"
  ( let 
    (  
      ( blk_score ( car  scores ) ) ;BLACK score
      ( wht_score ( cadr scores ) ) ;WHITE score
      winner                       ;WINNING COLOUR
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

#|
  Name: prompt-ai
  Description:
  "Prompt" the AI to perform their move. In reality, 
  the AI is not technically prompted, but this function 
  is named as such since it follows the convention of 
  preparing for a move, and handling if the player is 
  unable to perform a move successfully at the time.
  Paramaters:
    color - the color the AI is playing as
    board - game board
|#
( defun prompt-ai ( color board ) 
  "Prompt AI to perform their move"
  ( let 
    (
       row-col
    )

    ( setf temp board )
    ( cond
    	;iF there is a valid move, then perform minimax
      ( ( get-valid-moves board color)

      	;Get row-col of a given move
        ( setf row-col (make-move board color 4) )
        ;Perform Move If there is a valid one
        ( setf board ( place-disc color board (car row-col) (cadr row-col)) )
        ( setf board 
        	( car 
        		( flip-at color board (car row-col) (cadr row-col) ) 
      		) 
      	)
        ( format t "Here Is My Move: ~A ~A~%" (car row-col) (cadr row-col) )
      )
      ( T
        ( setf board temp )
        ( format t "No Moves Available...~%" )
      ) 
    )
    ;return the board
    board
  )
)


#|
  Name: prompt-turn
  Description:
  Informs the user that it is their turn, displays the game board for them, 
  and processes keyboard input to determine their next move. This function 
  also processes whether the human player doesn't have an available move, and 
  also handles invalid move options given from the keyboard input.
  Paramaters:
    player - color of the current player 
    board  - game board 
|#
( defun prompt-turn ( player board ) 
  "Inform user it is their turn, validate if the move they input is acceptable"
  ( let 
    ( 
      input
      row
      col
      temp-board
    )

    ;set temporary board
    (setf temp-board (copy-list board) )

    ;display the current board state
    ( print-board temp-board )

    ( cond

      ;pass player turn?
      ( ( pass-turn? player  board ) 
        ( format t "NO MOVES AVAILABLE FOR: ")
        ( print-player player )
        ( format t "~%")
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
          ;IF row and col are not numbers, illegal move
          ( ( or 
                ( not ( numberp row ) )
                ( not ( numberp col ) )
                
            )
              ( format t "~%Illegal Move. Please Try Again.~%" )
              ( prompt-turn player board )
          )
          ;Place the disc if legal move
          ( (legal-move? player  temp-board row col )
            ( setf board ( place-disc player  board row col ) )
            ( setf board ( car ( flip-at player board row col ) ) )
          )

          ;else, reprompt the player so they can try again.
          ( T 
            ( format t "~%Illegal Move. Please Try Again.~%" )
            ( prompt-turn player board )
          )
        )
      )
    )
    ;return the board on successful play
    board
  )
)

#|
  Name: place-disc
  Description:
  Place a disc in a given location.
  Paramaters:
    player - player color placing the disc
    board  - game board 
    row    - row in which disc is being placed 
    col    - column in which disc is being placed
|#
( defun place-disc ( player board row column ) 
  "Place a Disc in the specified location"
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
          ( setf (nth location board) 'B )
      )

      ;Place White Disc
      ( ( string= player 'W ) 
          ;place disc
          (setf (nth location board) 'W)
      )
    )
    ;return the board
    board
  )
)

#|
  Name: end-turn
  Description:
  Swap to the other player's turn. This only works for 
  human vs human mode.
  Paramaters:
    player - current player 
    board  - game board
|#
( defun end-turn ( player board )
  "Switch turns to the other player (human v human only)"
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


#|
  Name: count-legal-moves
  Description:
  Old function for counting valid legal moves a player can make. Basically all 
  instances in which this function was used have been replaced by Marcus's 
  get-valid-moves function since it's a lot less complicated and does the same 
  thing. However, I have left this in just in case.
  Paramaters:
    player - current player 
    board  - game board
|#
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

    ;Can probably just use marcus's function? it's nicer i think...
    (setf temp-board (copy-list board))

    ( loop for row from 1 to 8 do 
      ( loop for col from 1 to 8 do 
        ;convert row-col to location
        ( setf location 
            ( - ( + ( * ( - row 1 ) 8 ) col ) 1 ) 
        )
        ( cond 
          ;Check every empty location to determine legal moves
          ( ( string= "-" ( nth location temp-board ) )   
            ( cond 
              ;Check if spot is legal move, increment count if so
              ( ( legal-move? player temp-board row col ) 
                ( incf moves )
                (setf temp-board (copy-list board))
              )
            )
          )
        )
      )
    )
    ;return count of available moves
    moves
  )
)

#|
  Name: legal-move?
  Description:
  Determine if an attempted move is legal
  Paramaters:
    player - current player 
    board  - game board
    row    - row    containing attempted move
    col    - column containing attempted move
|#
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
        ( setf flips ( cadr ( flip-at player board row column ) ) )
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

#|
  Name: pass-turn?
  Description:
  Determine whether a player is required to forfeit their current turn.
  Paramaters:
    player - current player 
    board  - game board
|#
( defun pass-turn? ( player board ) 
  "Check if a player has to pass their turn"
  ( cond
    ;If there are legal moves, don't pass turn
    ( ( get-valid-moves board player )
      NIL
    )
    ;otherwise, Pass Turn
    ( T
      T
    )
  )
)

#|
  Name: check-flip-dirs
  Description:
  Determine if you've hit an edge when looking for a bracket piece for 
  flipping discs.
  Paramaters:
    row        - row    containing attempted move
    column     - column containing attempted move
    directions - the direction in which you're checking
|#
( defun check-flip-dirs ( row column directions )  
  "Verify possible flips are valid in the given directions"
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

#|
  Name: keep-moving?
  Description:
  Determine if you've hit an edge when looking for a bracket piece for 
  flipping discs. This specifically determines if you should keep 
  searching or give up.
  Paramaters:
    curr        - current position being checked on board
    direction   - current direction being checked on board
|#
( defun keep-moving? ( curr direction )
  "Checks whether function should still search for bracket piece"
  ( let 
    (
      location
      ( left-edge   '( 00 08 16 24 32 40 48 56 ) )
      ( right-edge  '( 07 15 23 31 39 47 55 63 ) )
      ( top-edge    '( 00 01 02 03 04 05 06 07 ) )
      ( bottom-edge '( 56 57 58 59 60 61 62 63 ) )
      ( moving t )
    )

    ( cond
      ;Check out of bounds
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
    moving
  )
)

#|
  Name: flip-at
  Description:
  Flip discs from the specified row column until you hit the bracket piece. 
  This function is very long because it first seeks out the bracket piece(s) 
  to validate their existence, then backtracks to flip all pieces between the 
  placed disc and the bracket piece. There is also a lot of legacy redundant 
  checks factored in from early human vs human implementation to ensure illegal 
  flips do not occur.
  Paramaters:
    player - player color
    board  - game board 
    row    - row in which piece had been placed to perform flip 
    column - column in which piece had been placed to perform flip
|#
( defun flip-at ( player board row column )
  "Flip at current location"
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
        (setf player-piece 'B)
        (setf opponent-piece 'W)
      )

      ( ( string= player 'W ) 
        (setf player-piece 'W)
        (setf opponent-piece 'B)
      )
    )

    ;set curr to the piece location 
    (setf curr location)
    
    ;For each direction in the list of directions (8 dirs)
    ( loop for dir in directions do 

      ;reset whether you've found a bracket
      ( setf found-bracket nil )
            
      ;increment curr in the direction you want to move
      (setf curr (+ curr dir))
      
      ( cond
        ;Short circuit to handle -1 or 64 out of bounds
        ( (or ( > curr 63) ( < curr 0 )) 
          nil
        )

        ;Found one of your opponent's pieces directly
        ;adjacent to the piece you wanna place, 
        ;start looking for a bracket.
        ( ( string= opponent-piece (nth curr board)  ) 
          (setf flip-list (append  flip-list (list curr)) )

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

                      ;if the bracket is bad, don't do flips
                      ( ( member -1000 flip-list )
                        ;( format t "Bad Bracket: No FLips~%" )
                      )

                      ;otherwise, process the flip list
                      ( t
                        ( dolist (q flip-list )
                          ;( format t "FLIPPING: ~A~%" q )
                          ( cond

                            ;Flip BLACK to White
                            ( ( string=  (nth q  board )  'B ) 
                                (setf  (nth q  board ) 'W )
                                ( incf num-flipped )
                            )

                            ;Flip White to Black
                            ( ( string=  (nth q  board )  'W ) 
                                (setf  (nth q  board ) 'B )
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
    ;returns both the board and the number of flipped tiles
    (list board num-flipped)
  )
)

#|
  Name: end-game?
  Description:
  Based on the board state, determine if the game should end now. Game 
  termination is determined when both players consecutively pass turn.
  Paramaters:
    board  - game board 
|#
( defun end-game? ( board )
  "Check if both players just passed turns, thus ending the game"

  ( cond
    ;if both players skip, then game is over
    (
      ( and 
        ( pass-turn? 'B board ) 
        ( pass-turn? 'W board ) 
      )
        T
    )

    ;otherwise, game isn't over
    ( t
        nil
    )
  )
)

#|
  Name: prompt-player-colour
  Description:
  Ask the player which color they want to play as. Black always moves first. 
  Only really important for human vs ai, since human vs human assumes two 
  human players, and ai vs ai has zero human interaction & Black AI moves first.
  Paramaters:
    nil
|#
( defun prompt-player-colour ()
  "Ask the person what colour they want to be"
  ( let 
    (  
      input
      choice    
    )

    ( format t "Would you like to play as " )
    ( format t "~c[37;40mBLACK~c[0m or " #\ESC #\ESC  )
    ( format t "~c[30;47mWHITE~c[0m ?~%" #\ESC #\ESC  )
    ( format t "(~c[37;40mBLACK~c[0m moves first) > " #\ESC #\ESC  )

    ;get user input...
    ( setf input ( read-line ) )

    ( with-input-from-string ( stream input )
      ;...and set the FIRST number as ROW...
      ( setf choice ( read stream NIL NIL ) )
    )

    ( cond
      ( ( numberp choice )
        ( prompt-player-colour )
      )
      ;Check if proper user input
      ( 
        ( or ( string= ( subseq (string-upcase choice ) 0 1 ) 'B )
             ( string= ( subseq (string-upcase choice ) 0 1 ) 'W ) 
        )
        ;Correct, pass this as player choice
        ( subseq (string-upcase choice ) 0 1 )
      )
      ;Otherwise, re-prompt because they entered something wrong
      ( T
        ( prompt-player-colour )
      )
    )
  )
)

#|
  Name: prompt-play-again?
  Description:
  Ask the player if they wish for revenge. Choosing yes will restart whichever 
  game type previous game was. Choosing no will return user to main menu, where 
  they can chose a different gametype.
  Paramaters:
    player   - player color
    gametype - gametype of previous game that just ended.
|#
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
    
    ( cond
      ;Reprompt if they put in a number on accident
      ( ( numberp choice )
        ( prompt-play-again? player gametype )
      )
      ;Restart game
      ( 
        ( or ( string= (string-upcase choice ) 'Y )
             ( string= (string-upcase choice ) 'YES)
        )

        ( cond
          ;CHECK GAMETYPE
          ( ( string= gametype "OTHELLO-HUMAN-VS-HUMAN" )
            ( othello-human-vs-human )
          )
          ( ( string= gametype "OTHELLO-AI-VS-AI" )
            ( othello-ai-vs-ai )
          )
          ( ( string= gametype "OTHELLO-HUMAN-VS-AI" )
            ( othello-human-vs-ai )
          )
        )
      )

      ;End Game
      ( 
        ( or ( string= (string-upcase choice ) 'N )
             ( string= (string-upcase choice ) 'NO)
        )
          ( othello )
      )

      ;else: Reprompt
      ( T
        ( prompt-play-again? player gametype )
      )
    )
  )
)

#|
  Name: prompt-gametype
  Description:
  Main menu prompting user for which gametype they would wish to play. The 
  user is also given the option to quit out of the program.
  Paramaters:
    player - player color
|#
( defun prompt-gametype ( &optional ( player nil ) ) 
  "Ask the person what game type they wish to play"
  ( let 
    (  
      input
      choice    
    )
  )

  ( format t "~%GAME MODES: ~%" )
  ( format t "[1] HUMAN VS HUMAN~%" )
  ( format t "[2] AI    VS AI   ~%" )
  ( format t "[3] HUMAN VS AI   ~%" )
  ( format t "[4] QUIT PROGRAM  ~%~%" )

  ( format t "Please select a gametype (1, 2, 3, or 4): ")
  ;get user input...
  ( setf input ( read-line ) )

  ( with-input-from-string ( stream input )
    ;...and set the FIRST number as ROW...
    ( setf choice ( read stream NIL NIL ) )
  )

  ( cond
    ;CHECK GAMETYPE
    ;FIRST handle if they aren't numbers...
    ( ( not ( numberp choice ) )
      ( format t "~%Please try again... ~%")
      ( prompt-gametype player )
    )

    ( ( = choice 1 )
      ( format t "~%Preparing: HUMAN VS HUMAN~%")
      ( othello-human-vs-human player )
    )
    ( ( = choice 2 )
      ( format t "~%Preparing: AI VS AI~%")
      ( othello-ai-vs-ai )
    )
    ( ( = choice 3 )
      ( format t "~%Preparing: HUMAN VS AI~%")
      ( othello-human-vs-ai player )
    )
    ( ( = choice 4 )
      ( format t "~%See ya.... ~%")
      ( values )
    )
    ;else: Reprompt
    ( T
      ( format t "~%Please try again... ~%")
      ( prompt-gametype player )
    )
  )
)