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

#|
  Name: othello
  Description:
  Displays game title card, and prompts user to select game mode.
  Paramaters:
    player - player color
|#
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
  ( prompt-gametype player )
)

#|
  Name: make-move
  Description:
  Tournament compliant movement function that returns a row - column
  pair when given a current board state. This function is where the 
  minimax search is called and performed. 
  Paramaters:
    position - current board state
    player   - which player is performing minimax
    ply      - how many turns the minimax will look up
|#
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

#|
  Name: othello-ai-vs-ai
  Description:
  Observe a game of othello in which the AI plays against itself. This game 
  mode was not explicitly required for this assignment, but has been included 
  anyways since ai-vs-ai has proven to be useful in testing and validating the 
  ai routines were working during development.
  Paramaters:
    board - starting state of the board
|#
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

#|
  Name: othello-human-vs-human
  Description:
  Play a game of othello in which both players are human. This game mode 
  was not explicitly required for this assignment, but has been included 
  anyways since human-vs-human has proven to be useful in testing and 
  validating the the game board rules for placing and flipping discs, as well 
  as determining legality of moves and other miscellaneous othello rule bases.
  Although the player parameter is supplied, Black always goes first and 
  since both players are humans, it is assumed that the player who wishes to 
  play as Black will have access to the keyboard first.
  Paramaters:
    player - player color provided by command parameter
    board  - starting state of the board
|#
( defun othello-human-vs-human  ( &optional 
                                  ( player nil ) 
                                  ( board ( copy-list (new-board) ) ) 
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
  ( print-board board ) 
  ( declare-winner (count-discs board ) )
  ( prompt-play-again? player "OTHELLO-HUMAN-VS-HUMAN" )
  ( values )
)

#|
  Name: othello-human-vs-ai
  Description:
  Play a game of othello in which one player is human and the other is an 
  AI written for this assignment. The AI used implements minimax with 
  alpha-beta pruning. The moves determined are based on heuristics found 
  in heuristic-funcs.lsp.
  Black always moves first, and the human player is given the opportunity to 
  choose whether they play as Black, or if they would like to go second as 
  the White player.
  Paramaters:
    player - player color. If nothing is passed in, player will be prompted.
    board  - starting state of the board
|#
( defun othello-human-vs-ai  ( &optional ( player nil ) 
                             ( board ( copy-list (new-board) ) )
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

#|
  Name: othello-init
  Description:
  Function called once prior to tournament for initialization purposes.
  This function is required for the class tournament, but as of the time 
  this documentation was written, this program does not require any 
  initialization beyond the loading in of all the different files in the 
  othello project.
  Paramaters:
    nil
|#
( defun othello-init ()
    "Function called once prior to tournament for initialization purposes"

)

#|--------------------------------------------------------------------------|#
#|                              MAIN FUNCTION                               |#
#|--------------------------------------------------------------------------|#

#|
  Name: main
  Description:
  Main function that handles interpreting command line arguments if the program 
  is called from bash.
  Paramaters:
    nil
|#
( defun main ()
  "Automatically calls the othello function when othello.lsp script is run."
  ( when ( = ( length *args* ) 1 )
    ( othello ( car *args* ) )
  )
)

( main )