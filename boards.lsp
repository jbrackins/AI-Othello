#|
                    ***** BOARDS.LSP *****

A number of sample boards for testing purposes.

Author:  J. Anthony Brackins, Marcus Haberling


Written Spring 2016 for CSC447/547 AI class.

Modifications: 

|#


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

( defparameter *board-two* 
                            '( - - - - - - - -
                               - - - - - - - - 
                               - - - - - - - - 
                               - - - W B - - - 
                               - - - B B - - - 
                               - - - - B - - - 
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
