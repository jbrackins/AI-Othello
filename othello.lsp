#|
                    ***** OTHELLO.LSP *****

Author:  J. Anthony Brackins, Marcus Haberling


Written Spring 2016 for CSC447/547 AI class.

Modifications: 

|#


( defun make-move ( position player ply ) 
	"function to allow Othello programs interact in computer tournament"

)


( defun othello ( &optional ( player nil ) ) 
	"Starts up a game of othello."

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
