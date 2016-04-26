#|
                    ***** PROMPT.LSP *****

Functions originally in game.lsp that were moved to this file to cut down on 
file size. This file contains specifically "prompt" functions, which 
typically request specified user input.

Author:  J. Anthony Brackins, Marcus Haberling

Written Spring 2016 for CSC447/547 AI class.

Modifications: 

|#

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