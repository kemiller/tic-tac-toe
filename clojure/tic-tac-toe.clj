
; Defining the players
(def players (cycle ["X" "O"]))

; Setting up the board
(def *starting-board* 
  [[1 2 3]
   [4 5 6]
   [7 8 9]])

(defn space-available? [space board]
    (some (fn [row] (some #(= %1 space) row)) board))

; Moving -- returns a transformed board
(defn move [player space board]
	  (let [move-in-row
			 (fn [accum row]
				 (cond
				   ; if we reach the end, that's it
				   (empty? row) accum
				   ; if we have found the space, take it 
				   (= space (first row)) (concat accum (cons player (rest row)))
				   ; if not, keep going
				   true (recur (concat accum (list (first row))) (rest row))))]
	 (map move-in-row '() board)))
        
(defn space->string [space]
 (if (number? space)
  (str "(" space ")")
  (str " " space " ")))

(defn row->string [row] (str (interpose "|" (map space->string row) )))

(defn board->string [board]
    (str (interpose "\n---+---+---\n" (map row->string board))))

(defn third [seq] (nth seq 2))

(defn winner? [board]
  (let [all-eq?  (fn [seq] (= (first seq) (second seq) (third seq)))]
	 (contains? true
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
            (all-eq? (list (first (first board))
                           (second (second board)) 
                           (third (third board))))
            (all-eq? (list (third (first board)) 
                           (second (second board)) 
                           (first (third board))))))))

(defn full? [board]
  (not (some number? (concat (first board) (second board) (third board)))))

(defn print-board [board]
	(print (str "\n" (board->string board) "\n\n")))

(defn play [board players]
  (do
    (print-board board)
    (let [
		  player (first players)
		  winning-player (winner? board)
		  ]
      (cond
        winning-player (print (str winning-player " Wins!\n"))
        (full? board) (print "It's a Draw!\n")
        true (do
                (print (str "Select a square, " player ": "))
                (let [answer (read)]
                  (if (and answer (space-available? answer board)) 
                      (play (move player answer board) (next players))
                      (play board player))))))))
 
(play *starting-board* players)
 
 
 
 
 
