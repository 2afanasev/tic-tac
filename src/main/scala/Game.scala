import scala.annotation.tailrec

sealed trait Square {
  def map(transformer: Line => Line): Square
}

class Square3x3(values: Array[Line] = Array(new Line3x(1),new Line3x(2),new Line3x(3))) extends Square {
  override def map(transformer: Line => Line): Square3x3 = {
    @tailrec
    def tailrec(n: Int = 0, acc: Array[Line] = Array.empty): Array[Line] = 
        if(n == values.length) acc
        else tailrec(n + 1, acc :+ transformer(values(n)))
    new Square3x3(tailrec())
  }
  override def toString(): String = {
    @tailrec
    def tailrec(n: Int = 0, acc: String = ""): String = 
      if(n == values.size) acc
      else tailrec(n + 1, acc + values(n) + "\n")
    tailrec()
  }
}

sealed trait Line {
  val id: Int
  def map(transformer: Slot => Slot): Line
}

class Line3x(val id: Int, slots: Array[Slot] = Array(new Slot("A"), new Slot("B"), new Slot("C"))) extends Line {
  def map(transformer: Slot => Slot): Line = {
    @tailrec
    def tailrec(n: Int = 0, acc: Array[Slot] = Array.empty): Array[Slot] = 
        if(n == slots.length) acc
        else tailrec(n + 1, acc :+ transformer(slots(n)))
    new Line3x(id, tailrec())
  }
  override def toString = {
    @tailrec
    def tailrec(n: Int = 0, acc: String = "|"): String =
      if(n == slots.size) acc
      else tailrec(n + 1, acc + slots(n) + "|")
    tailrec()
  }
}

case class Slot(id: String, value: Figure = Empty) {
  override def toString: String = s"$value"
}

sealed trait Figure

case class X() extends Figure {
  override def toString(): String = "X"
}
case class O() extends Figure {
  override def toString(): String = "O"
}
case object Empty extends Figure {
  override def toString(): String = " "
}

trait GameException
trait ErrorException extends GameException

class IncorrectValueException extends RuntimeException with ErrorException
class AlreadyInicializedException extends RuntimeException with ErrorException
class GameOverException extends RuntimeException with GameException