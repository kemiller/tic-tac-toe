
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
	  (letfn [(move-in-row 
			   ([accum row]
				 (cond
				   ; if we reach the end, that's it
				   (empty? row) accum
				   ; if we have found the space, take it 
				   (= space (first row)) (concat accum (cons player (rest row)))
				   ; if not, keep going
				   true (recur (concat accum (list (first row))) (rest row))))
				 ([row] (move-in-row '() row)))]
		(map move-in-row board)))

(defn space->string [space]
	  (if (number? space)
		(str "(" space ")")
		(str " " space " ")))

(defn row->string [row] (apply str (interpose "|" (map space->string row) )))

(defn board->string [board]
	  (apply str (interpose "\n---+---+---\n" (map row->string board))))

(defn third [seq] (nth seq 2))

(defn winner? [board]
	  (or
		; rows
		(apply = (first board))
		(apply = (second board))
		(apply = (third board))
		; columns
		(apply = (map first board))
		(apply = (map second board))
		(apply = (map third board))
		; diagonals
		(= (first (first board))
		   (second (second board))
		   (third (third board)))
		(= (third (first board))
		   (second (second board))
		   (first (third board)))))

(defn full? [board]
	  (not (some number? (mapcat identity board))))

(defn print-board [board]
	  (do
		(newline)
		(print (board->string board))
		(newline)
		(newline)))

(defn play [board players]
	  (do
		(print-board board)
		(let [ player (first players) ]
		  (cond
			(winner? board) (print (str (first (next players))  " Wins!\n"))
			(full? board) (print "It's a Draw!\n")
			true (do
				   (print (str "Select a square, " player ": "))
				   (flush)
				   (let [answer (read)]
					 (if (and answer (space-available? answer board)) 
					   (recur (move player answer board) (next players))
					   (recur board players))))))))

(play *starting-board* players)





