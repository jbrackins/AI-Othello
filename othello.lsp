#|
                    ***** OTHELLO.LSP *****

Author:  J. Anthony Brackins, Marcus Haberling


Written Spring 2016 for CSC447/547 AI class.

Modifications: 

|#

#|--------------------------------------------------------------------------|#
#|                               Files Loaded                               |#
#|--------------------------------------------------------------------------|#

( load 'print-funcs )
( load 'game        )
( load 'minimax     )

#|--------------------------------------------------------------------------|#
#|                         Tournament Functions                             |#
#|--------------------------------------------------------------------------|#

( defun make-move ( position player ply ) 
  "function to allow Othello programs interact in computer tournament"
  (car (car (cdr (minimax position ply player t))))

)


( defun othello ( &optional ( player nil ) ) 
	"Starts up a game of othello."

	;just printing out the arg just to verify CLI is workin
	(print player)
)

( defun othello-two-players ( &optional ( player nil ) ) 
	"Starts up a game of othello where both players are human."

	;just printing out the arg just to verify CLI is workin
	(print player)
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
