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

This file also contains various other important print functions as well.
 
Author:  J. Anthony Brackins, Marcus Haberling


Written Spring 2016 for CSC447/547 AI class.

Modifications: 

|#


#|
  Name: print-title
  Description:
  Print out the title card for our program!
  Paramaters:
    nil
|#
( defun print-title () 
  "Print OTHELLO Game Title"

  ( format t "~%" )
  ( format t " **********************************************************~%" )
  ( format t " *                                                        *~%" )
  ( format t " *   0000   000080000  8    8  80000  8     8      0000   *~%" )
  ( format t " *  0    0      8      8    8  8      8     8     0    0  *~%" )
  ( format t " *  0    0      8      8    8  8      8     8     0    0  *~%" )
  ( format t " *  0    0      8      800008  80000  8     8     0    0  *~%" )
  ( format t " *  0    0      8      8    8  8      8     8     0    0  *~%" )
  ( format t " *  0    0      8      8    8  8      8     8     0    0  *~%" )
  ( format t " *   0000       8      8    8  80000  80000 80000  0000   *~%" )
  ( format t " *                                                        *~%" )
  ( format t " *                                                        *~%" )
  ( format t " *              -Julian Brackins, Marcus Haberling, 2016  *~%" )
  ( format t " *                                                        *~%" )
  ( format t " **********************************************************~%" )
  ( values )
)

#|
  Name: print-board
  Description:
  Print out the game board at its current state. Additional formatting has 
  been implemented so that blank spaces are green dashes, Black Discs are 
  white text on black background, and White Discs are black text on white 
  background.
  Paramaters:
    position - current board state
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
        ( format t " ~%" ) 
        ;( format t " ~A~%" ( - end 1 ) ) 
        ;increment the iterators
        ( setf start end )
        ( setf end ( + end 8 ) )
        ( incf row )
      ) 
    ( format t "~%" ) 
    ( values )
  )
)

#|
  Name: print-player
  Description:
  Print out player name, either "BLACK" or "WHITE", with special ANSI escape 
  formatting. ANSI escape code formatting reference:
  https://en.wikipedia.org/wiki/ANSI_escape_code
  Paramaters:
    player - player color
|#
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

#|
  Name: print-square
  Description:
  Print out board square, with special ANSI escape 
  formatting. Blank spaces are green dashes, Black Discs are 
  white text on black background, and White Discs are black text on white 
  background. ANSI escape code formatting reference:
  https://en.wikipedia.org/wiki/ANSI_escape_code
  Paramaters:
    square - particular square on the board
|#
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

#|
  Name: test-print-board
  Description:
  Quick validation function to verify that print-board worked.
  Paramaters:
    nil
|#
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
