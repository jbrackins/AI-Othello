#|
                    ***** PRINT-FUNCS.LSP *****

Routines for Printing out Othello board in the following format:
  1 2 3 4 5 6 7 8
1 - - - - - - - -
2 - - - - - - - -
3 - - - - - - - -
4 - - - W B - - -
5 - - - B W - - -
6 - - - - - - - -
7 - - - - - - - -
8 - - - - - - - -

Where:
"-" is a blank location
"B" is a black piece
"W" is a white piece 

This file also contains various other print functions as well.
 
Author:  J. Anthony Brackins, Marcus Haberling


Written Spring 2016 for CSC447/547 AI class.

Modifications: 

|#

( defun print-board ( position ) 
  "Print out the current state of the board"
  ( let 
    (  
      ( board position ) 
      ( start 0 ) ;start iterator for SUBSEQ
      ( end   8 ) ;end iterator for SUBSEQ
      ( row   1 )    ;current row being printed
      ( square )
    )
    ;print out the column labels
    ( format t "~%  1 2 3 4 5 6 7 8~%" ) 
      ( loop while ( < row 9 ) do

        ;print row
        ( format t "~A " row ) 
        ;loop and print each square
        ( loop for square in ( subseq board start end ) do 
            ;print a square with formatting
            ( print-square square )
        )
        ;Go to the next line (Optional: print out the value at end of line)
        ( format t " ~A~%" ( - end 1 ) ) 
        ;increment the iterators
        ( setf start end )
        ( setf end ( + end 8 ) )
        ( incf row )
      ) 
    ( format t "~%" ) 
    ( values )
  )
)

( defun print-player ( player )
  "Print Player name with formatted colour"
  ( cond

    ;Black
    ( ( string= player 'B ) 
      ( format t  "~c[37;40mBLACK~c[0m " #\ESC #\ESC)
    )

    ;White
    ( ( string= player 'W ) 
      ( format t  "~c[30;47mWHITE~c[0m " #\ESC #\ESC)
    )
  )
)

( defun print-square ( square )
  "Print a single square of the board with proper colour formatting"
  ( cond
    ;Black
    ( ( string= square "B" ) 
        ( format t  "~c[37;40m~A~c[0m " #\ESC square #\ESC)
    )
    ;White
    ( ( string= square "W" ) 
        ( format t  "~c[30;47m~A~c[0m " #\ESC square #\ESC)
    )
    ;Blank should be green
    (  ( string= square "-" ) 
        ( format t  "~c[32m~A~c[0m " #\ESC square #\ESC)
    ) 
    ;else
    ( T
        ( format t "~A " square )
    )
  )
)

( defun test-print-board () 
  "Verify print-board is working"
  ( let 
    ( 
      board 
    )
  
    ( setf board 
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

    ( print-board board )
  )
)
