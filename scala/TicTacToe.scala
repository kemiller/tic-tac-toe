
import java.io.{BufferedReader, InputStreamReader}

object TicTacToe {
    
  private val input = new BufferedReader(new InputStreamReader(System.in))

  def main(args: Array[String]) {
  
    var result: Result = Continue(X)

    while (true) {

      println
      println(board)
      println
      print(result)

      result match {
        case Continue(player) => {
            val space = readAnswer
            if (board.spaceAvailable(space)) {
              board.place(space, player)
              result = board.checkResult(player)
            }
        }
        case _ => return
      }

    }
  }

  def readAnswer: Int = {
    try {
      input.readLine().toInt
    } catch {
      case nfe: NumberFormatException => 0
    }
  }
}

abstract class Player {
  def next: Player
}
case object X extends Player {
  override def toString() = "X"
  def next = O
}
case object O extends Player {
  override def toString() = "O"
  def next = X
}

abstract class Space
case class Empty(n: Int) extends Space { 
  override def toString() = "(" + n + ")"
}
case class Filled(player: Player) extends Space {
  override def toString() = " " + player + " "
}

abstract class Result
case class Win(player: Player) extends Result {
  override def toString() = "" + player + " Wins!\n"
}
case object Draw extends Result {
  override def toString() = "It's a Draw!\n"
}
case class Continue(player: Player) extends Result {
  override def toString() = "Select a square, " + player + ": "
}

object board {
  private val spaces = Array[Space](
    Empty(1),Empty(2),Empty(3),
    Empty(4),Empty(5),Empty(6),
    Empty(7),Empty(8),Empty(9)
  )

  def spaceAvailable(n: Int): Boolean = {
    for (space <- spaces) {
      space match {
        case Empty(m) if m == n => return true
        case _ => ()
      }
    }
    return false
  }

  def full: Boolean = {
    for (space <- spaces) {
      space match { 
        case Empty(_) => return false
        case _ => ()
      }
    }
    return true;
  }

  def checkResult(player: Player): Result = {
    for (row <- rows) {
      row match {
        case Array(Filled(p),Filled(q),Filled(r)) if p == q && q == r => {
          return Win(p)
        }
        case _ => ()
      }
    }
    if (full) return Draw
    return Continue(player.next)
  }

  def place(n: Int, player: Player) {
    spaces(n-1) = Filled(player)
  }

  private def get(n: Int): Space = spaces(n-1)

  private def rows: Array[Array[Space]] = {
    Array[Array[Space]](
      spaces.slice(0,3),
      spaces.slice(3,6),
      spaces.slice(6,9),

      Array(get(1),get(4),get(7)),
      Array(get(2),get(5),get(8)),
      Array(get(3),get(6),get(9)),

      Array(get(1),get(5),get(9)),
      Array(get(3),get(5),get(7)))
  }

  override def toString() = {
    spaces(0) + "|" + spaces(1) + "|" + spaces(2) +
    "\n---+---+---\n" +
    spaces(3) + "|" + spaces(4) + "|" + spaces(5) +
    "\n---+---+---\n" +
    spaces(6) + "|" + spaces(7) + "|" + spaces(8)
  }
}

  




