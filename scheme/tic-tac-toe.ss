
#lang scheme

; Defining the players
(define o 'O)
(define x 'X)

(define (other-player player)
  (case player
    ((X) o)
    ((O) x)))

; Setting up the board
(define *starting-board* 
  '((1 2 3)
    (4 5 6)
    (7 8 9)))

(define (space-available? space board)
  (let ((member-space (lambda (row) (member space row))))
    (ormap member-space board)))

; Moving -- returns a transformed board
(define (move player space board)
  (letrec ((move-in-row
            (lambda (row)
              (cond
                ; if we reach the end, that's it
                [(null? row) '()]
                ; if we have found the space, take it 
                [(eqv? space (first row)) (cons player (rest row))]
                ; if not, keep going
                [else (cons (first row) (move-in-row (rest row)))]))))
        (map move-in-row board)))
        
(define (board->string board)
  (let* ([space->string 
          (lambda (space)
            (cond 
              [(number? space) (string-append "(" (number->string space) ")")]
              [(symbol? space) (string-append " " (symbol->string space) " ")]
              [else ""]))]
         [row->string
          (lambda (row) (string-append* (add-between (map space->string row) "|")))])
    (string-append* (add-between (map row->string board) "\n---+---+---\n"))))

(define (winner? board)
  (let ([all-eq?
         (lambda (lst) 
           (foldl (lambda (x y) (if (eq? x y) x #f)) (first lst) (rest lst)))])
    (findf (lambda (x) x)
           (list
            ; rows
            (all-eq? (first board))
            (all-eq? (second board))
            (all-eq? (third board))
            ; columns
            (all-eq? (map first board))
            (all-eq? (map second board))
            (all-eq? (map third board))
            ; diagonals
            (all-eq? (list (first (first board)) (second (second board)) (third (third board))))
            (all-eq? (list (third (first board)) (second (second board)) (first (third board))))))))

(define (full? board)
  (not (findf number? (flatten board))))

(define (display-board board)
  (begin
    (newline)
    (display (board->string board))
    (newline)
    (newline)))

(define (play board player)
  (begin
    (display-board board)
    (let ((winning-player (winner? board)))
      (cond
        [winning-player (display (string-append (symbol->string winning-player) " Wins!\n"))]
        [(full? board) (display "It's a Draw!\n")]
        [else (begin
                (display (string-append "Select a square, " (symbol->string player) ": "))
                (let ((answer (string->number (regexp-replace* #px"\\s*" (read-line) ""))))
                  (if (and answer (space-available? answer board)) 
                      (play (move player answer board) (other-player player))
                      (play board player))))]))))
 
(play *starting-board* x)
 
 
 
 
 