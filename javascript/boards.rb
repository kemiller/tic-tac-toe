
module Boards

STARTING_BOARD = %{
(1)|(2)|(3)
---+---+---
(4)|(5)|(6)
---+---+---
(7)|(8)|(9)

Select a square, X: }

X_ON_1 = %{
 X |(2)|(3)
---+---+---
(4)|(5)|(6)
---+---+---
(7)|(8)|(9)

Select a square, O: }

X_ON_5_O_ON_9 = %{
(1)|(2)|(3)
---+---+---
(4)| X |(6)
---+---+---
(7)|(8)| O 

Select a square, X: }

X_WINS_1ST_HORIZ_RANK = %{
 X | X | X 
---+---+---
 O | O |(6)
---+---+---
(7)|(8)|(9)

X Wins!
}

X_WINS_2ND_HORIZ_RANK = %{
(1)|(2)|(3)
---+---+---
 X | X | X 
---+---+---
 O | O |(9)

X Wins!
}

X_WINS_3RD_HORIZ_RANK = %{
(1)|(2)|(3)
---+---+---
 O | O |(6)
---+---+---
 X | X | X 

X Wins!
}

X_WINS_1ST_VERT_RANK = %{
 X | O |(3)
---+---+---
 X | O |(6)
---+---+---
 X |(8)|(9)

X Wins!
}

X_WINS_2ND_VERT_RANK = %{
 O | X |(3)
---+---+---
(4)| X | O 
---+---+---
(7)| X |(9)

X Wins!
}

X_WINS_3RD_VERT_RANK = %{
 O |(2)| X 
---+---+---
 O |(5)| X 
---+---+---
(7)|(8)| X 

X Wins!
}

X_WINS_SLASH_DIAG = %{
 O |(2)| X 
---+---+---
(4)| X |(6)
---+---+---
 X |(8)| O 

X Wins!
}

X_WINS_BACKSLASH_DIAG = %{
 X |(2)| O 
---+---+---
(4)| X |(6)
---+---+---
 O |(8)| X 

X Wins!
}

O_WINS_1ST_HORIZ_RANK = %{
 O | O | O 
---+---+---
 X | X |(6)
---+---+---
(7)|(8)| X 

O Wins!
}

DRAW_DIAGONAL_Y = %{
 X | O | X 
---+---+---
 O | O | X 
---+---+---
 X | X | O 

It's a Draw!
}

DRAW_VERTICAL_Y = %{
 O | X | O 
---+---+---
 X | O | X 
---+---+---
 X | O | X 

It's a Draw!
}

end

