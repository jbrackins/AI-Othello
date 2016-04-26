#|
                    ***** OTHELLO.LSP *****

Lisp based Othello program designed to investigate AI game playing techniques. 

# Othello Game Description 
Othello, also known as Reversi, is a game played on an 8x8 grid with 64 discs 
that are black on one side and white on the other. Based on the following 
start position:

    - - - - - - - -
    - - - - - - - - 
    - - - - - - - - 
    - - - W B - - - 
    - - - B W - - - 
    - - - - - - - - 
    - - - - - - - - 
    - - - - - - - - 

the two players (BLACK and WHITE) alternate turns placing discs on the board. 
In order for a valid turn to be made, discs must be placed to surround enemy 
pieces. Whenever a row, column, or diagonal of contiguous white pieces are 
surrounded on both ends by black pieces, the white pieces are "flipped" to the 
black side, making them black piecs. ( The opposite applies for flipping to 
white pieces, naturally. ) This is referred to as "flanking", or "bracketing".

In the following demonstration, The black player moves first, places a disc in 
row 6, column 5. This causes the white disc in row 5, column 5 to flip to the 
black side.

    - - - - - - - -     - - - - - - - -     - - - - - - - - 
    - - - - - - - -     - - - - - - - -     - - - - - - - - 
    - - - - - - - -     - - - - - - - -     - - - - - - - - 
    - - - W B - - -  => - - - W B - - -  => - - - W B - - - 
    - - - B W - - -  => - - - B W - - -  => - - - B B - - -  
    - - - - - - - -     - - - - B - - -     - - - - B - - - 
    - - - - - - - -     - - - - - - - -     - - - - - - - - 
    - - - - - - - -     - - - - - - - -     - - - - - - - - 

Whenever a player cannot place a disc that will bracket enemy pieces, that 
player must forfeit their current turn.

The game continues until either the board is filled, or once a scenario occurs 
in which neither player can perform a legal move. At this point, the number of 
discs are counted for each side and the player with the most discs is the
victor.

# Program Description
For this project, a Lisp program has been implemented for the Othello game. 
There are options for human vs human, human vs ai, and ai vs ai. The AI 
implemented for this program is designed to investigate AI game playing 
techniques. In particular, this program uses the minimax strategy with 
alpha-beta pruning.

# Minimax
Minimax is an algorithm that works well for two-player board games, and exists 
as a model for making moves based on knowledge of what the other player will do. 
This algorithm opperates on the truism "What is good for my opponent is bad for 
me". The general idea of minimax is the 
following:
  1. Look at all available moves
  2. Assign a rating to each board position that results form each move
  3. Pick the move with the [minimum/maximum] point rating.

This algorithm alternates step 3 for minimum and maximum for the player 
[maximum] and their opponent [minimum]. One of the benefits of this method is 
that the point ratings work for both the player moves and the opponent moves; 
alternating looking at minimum during opponent's turn and maximum during player 
turn is sufficient. Minimax is performed by creating a graph of available moves 
and performing depth-first search.

( Minimax Description Source: Land of Lisp, Conrad Barski )

# Alpha-beta Pruning
Alpha beta pruning is a search algorithm that decreases the number of nodes 
needed to be evaluated by the minimax algorithm. This algorithm stops 
evaluating a particular move when at least one possibility has been found 
that renders the move to be worse than a previously examined move available 
for the current player. As a result, the returned solution from minimax without 
alpha-beta pruning will be identical to the solution from minimax with 
alpha-beta pruning. The major difference is the amount of time saved by 
reducing the number of expanded nodes in the search algorithm.

( Alpha-beta Pruning Source: https://en.wikipedia.org/wiki/Alpha-beta_pruning )

# Program Usage
* Command Line Usage:
    
    clisp othello.lsp <player> (Black or White)

* CLISP Usage:

    (load 'othello)
    ( othello [player] )

# Othello Tournament
For the Spring 2016 CSC 447/547 course, we are holding an Othello tournament 
in which our program will compete against the programs written by other 
students in the course. in order to facilitate the programs playing against one 
another, the following torunament functions have been supplied:

( make-move position player ply ) 

This function takes the current board position, the current player who has the 
next move, and the depth of search (or ply, to look ahead in the game tree). 
It returns a ( row col ) list that specifies the move selected by the minimax 
function, or NIL if no legal move exists.

( othello-init )

Another tournament function is othello-init, which takes no arguments. This 
function is called once, prior to the start of tournament play. Initialization 
code may be placed in this routine if necessary, but our implementation does 
not use this function at all.

# References
A fair bit of research was put into this program for learning the rules to 
Othello and gaining an understanding for implementing our strategies. Some 
sites used for reference:
* Reversi wikipedia page: https://en.wikipedia.org/wiki/Reversi
* Othello wiki-how  page: http://www.wikihow.com/Play-Othello
* Reversi Strategy Guide: http://www.samsoft.org.uk/reversi/strategy.htm
* A-B Pruning wikipedia page: https://en.wikipedia.org/wiki/Alpha-beta_pruning
* Land of Lisp Programming Book: http://dl.acm.org/citation.cfm?id=1951916

Author:  J. Anthony Brackins, Marcus Haberling

Written Spring 2016 for CSC447/547 AI class.

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
    ( loop while ( not ( end-game? board ) ) do 
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
  ( loop while ( not ( end-game? board ) ) do 
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

    ( loop while ( not ( end-game? board ) ) do 
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