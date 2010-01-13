       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TicTacToe.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 CurrentPlayer     PIC A VALUE "X".
       01 CurrentMove       PIC 9(10).
       01 RowSeparator      PIC X(11) VALUE "---+---+---". 

      * The board, for calculation purposes
       01 CurrentBoard.
           02 CurrentBoardValues        PIC X(9) VALUE "123456789".
           02 CurrentBoardTable REDEFINES CurrentBoardValues.
               03 Cell OCCURS 9 TIMES PIC X.

      * The board, for display purposes
       01 BoardForDisplay.
           02 BoardValuesForDisplay.
               03 RowOne        PIC X(11) VALUE "(1)|(2)|(3)".
               03 FILLER        PIC X.
               03 RowTwo        PIC X(11) VALUE "(4)|(5)|(6)".
               03 FILLER        PIC X.
               03 RowThree      PIC X(11) VALUE "(7)|(8)|(9)".
               03 FILLER        PIC X.

           02 FILLER REDEFINES BoardValuesForDisplay.
               03 DisplayCell   OCCURS 9 TIMES PIC X(4).

       01 GameOver          PIC X VALUE 'F'.

       PROCEDURE DIVISION.
       Begin.
           PERFORM WITH TEST AFTER UNTIL GameOver EQUAL 'T'
               PERFORM DisplayBoard
               DISPLAY "Select a square, " CurrentPlayer ": "
                   WITH NO ADVANCING
               ACCEPT  CurrentMove
               IF CurrentMove > 0 AND CurrentMove < 10 AND
                       Cell(CurrentMove) NUMERIC
                   MOVE CurrentPlayer TO Cell(CurrentMove)
                   CALL "FormatCell" USING BY CONTENT CurrentPlayer
                       BY REFERENCE DisplayCell(CurrentMove)
                   PERFORM CheckForWin
                   PERFORM CheckForDraw
                   PERFORM SwitchPlayer
               END-IF
           END-PERFORM.
           STOP RUN.

       DisplayBoard.
           DISPLAY ""
           DISPLAY RowOne
           DISPLAY RowSeparator
           DISPLAY RowTwo
           DISPLAY RowSeparator
           DISPLAY RowThree
           DISPLAY "".
           
       CheckForWin.
           IF Cell(1) EQUAL Cell(2) AND Cell(2) EQUAL Cell(3)
                   OR Cell(4) EQUAL Cell(5) AND Cell(5) EQUAL Cell(6)
                   OR Cell(7) EQUAL Cell(8) AND Cell(8) EQUAL Cell(9)
                   OR Cell(1) EQUAL Cell(4) AND Cell(4) EQUAL Cell(7)
                   OR Cell(2) EQUAL Cell(5) AND Cell(5) EQUAL Cell(8)
                   OR Cell(3) EQUAL Cell(6) AND Cell(6) EQUAL Cell(9)
                   OR Cell(1) EQUAL Cell(5) AND Cell(5) EQUAL Cell(9)
                   OR Cell(3) EQUAL Cell(5) AND Cell(5) EQUAL Cell(7)
               PERFORM DisplayBoard
               DISPLAY CurrentPlayer " Wins!"
               SET GameOver TO 'T'
           END-IF.

       CheckForDraw.
           IF CurrentBoard ALPHABETIC AND GameOver NOT EQUAL 'T'
               PERFORM DisplayBoard
               DISPLAY "It's a Draw!"
               SET GameOver TO 'T'
           END-IF.

       SwitchPlayer.
           IF CurrentPlayer EQUAL "X" THEN
               SET CurrentPlayer TO "O"
           ELSE
               SET CurrentPlayer TO "X"
           END-IF.


       IDENTIFICATION DIVISION.
       PROGRAM-ID. FormatCell
       DATA DIVISION.
       LINKAGE SECTION.

       01 CellValue             PIC X.

       01 CellRepresentation.
           02 LeftPad           PIC X.
           02 ContentSpace      PIC X.
           02 RightPad          PIC X.
           02 FILLER            PIC X.

       PROCEDURE DIVISION USING CellValue, CellRepresentation.
       Begin.
           MOVE CellValue to ContentSpace
           IF CellValue NUMERIC
               MOVE "(" TO LeftPad
               MOVE ")" TO RightPad
           ELSE
               MOVE " " TO LeftPad
               MOVE " " TO RightPad
           END-IF.
           EXIT PROGRAM.

       END PROGRAM FormatCell.

       END PROGRAM TicTacToe.

           


