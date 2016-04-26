#|
                    ***** OTHELLO.LSP *****

Author:  J. Anthony Brackins, Marcus Haberling


Written Spring 2016 for CSC447/547 AI class.

Modifications: 

|#

#|--------------------------------------------------------------------------|#
#|                               Files Loaded                               |#
#|--------------------------------------------------------------------------|#

( load 'print-funcs     )
( load 'game            )
( load 'minimax         )
( load 'boards          )
( load 'board-funcs     )
( load 'heuristic-funcs )

#|--------------------------------------------------------------------------|#
#|                         Tournament Functions                             |#
#|--------------------------------------------------------------------------|#

( defun othello ( &optional ( player nil ) ) 
  "Starts up a game of othello."
  ( print-title )
  ( cond
    ;Prompt for colour if none given
    ( 
      ( not ( null player ) )
      ( setf player ( subseq player 0 1 ) )
    )
  )
  ;( othello-human-vs-human player )
  ( prompt-gametype player )
)

( defun make-move ( position player ply ) 
  "function to allow Othello programs interact in computer tournament."
  ( let 
    (
      ( old position )
      (move  (caadr ( minimax position ply player t ) ) )
    )

    ( cond

      ;Make-move should return NIL if there are no valid moves
      ( ( not ( get-valid-moves position player ) )
        nil
      )

      ;If there was a valid move, return the (row col) list
      ( T
        ( loc-to-row-col  ( find-location old move )  )
      )
    ) 
  )
)

( defun othello-ai-vs-ai ( &optional ( board ( copy-list (new-board) ) ) ) 
  "Othello AI vs Othello AI"
  ( let 
    (
     (color 'B)
     row-col
    )
    ;Alternate between black and white AI until done
    ( loop while ( not 
                  ( and (end-game? color board) 
                        (end-game? ( other-color color ) board) ) ) do 
      ( print-board board ) 
      ( setf board ( prompt-ai color board ) )
      ( setf color ( other-color color ) )
    )
    ( print-board board ) 
    ( declare-winner (count-discs board ) )
    ( prompt-play-again? color "OTHELLO-AI-VS-AI" )
  )
)

( defun othello-human-vs-human  ( &optional 
                                  ( player nil ) 
                                  ( board ( new-board ) ) 
                                ) 
  "Starts up a game of othello where both players are human."

  ;Prompting for color doesn't really matter, just hand
  ;the computer to whoever wants to go first
  ( loop while ( not 
                  ( and (end-game? 'W board) 
                        (end-game? 'B board) ) ) do 

    ( setf board ( prompt-turn 'B board ) )
    ( setf board ( prompt-turn 'W board ) )    
  )
  ( prompt-play-again? player "OTHELLO-HUMAN-VS-HUMAN" )
  ( values )
)

( defun othello-human-vs-ai  ( &optional ( player nil ) 
                            ( board (new-board) ) 
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

    ( loop while ( not 
                    ( and (end-game? player board) 
                          (end-game? ai     board) ) ) do 
      ( cond

          ;Player = Black; AI = WHITE
          ( ( string= player 'B ) 
              ( setf board ( prompt-turn player board ) )
              ( print-board board )
              ( setf board ( prompt-ai   ai     board ) )
          )
          ;Player = WHITE; AI = BLACK
          ( ( string= player 'W ) 
              ( setf board ( prompt-ai   ai     board ) )
              ( setf board ( prompt-turn player board ) )
              ( print-board board )
          )
      )
    )
    ( declare-winner (count-discs board ) )
    ( prompt-play-again? player "OTHELLO-HUMAN-VS-AI" )
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