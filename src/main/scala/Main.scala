import scala.io.StdIn.readLine;

object Game extends App {

  private val pattern = "[1-3][A-C]".r

  def start(square: Square = new Square3x3()): Unit = {
    println(square)
    start(
      try {
        square.map(step(X(), readLine()))
      } catch {
        case gameException: GameException => gameException match {
          case errorException: ErrorException => errorException match {
            case incorrectValue: IncorrectValueException => {
              println(incorrectValue.getMessage())
              square
            } 
          }
          case gameOver: GameOverException => {
            println(gameOver.getMessage())
            new Square3x3()
          }
        } 
        case _: Throwable => {
          println("unexpected error, please try again")
          square
        }
      }
    )
  }

  val step = (figure: Figure, str: String) => {
    val value: (Int, String) = 
      if(pattern.matches(str)) str.split("") match {
        case Array(str1, str2) => (str1.toInt, str2)
        case _ => throw new RuntimeException
      } else throw new RuntimeException
    
    (line: Line) => if(line.id == value._1) line.map(slot => if(slot.id == value._2) slot match {
      case Slot(_ , Empty) => new Slot(slot.id, figure)
      case _ => throw new RuntimeException
    } else slot) else line
  }

  start()
}