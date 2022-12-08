import scala.io.Source
import scala.math.{cos, pow, sqrt}
import java.io.InputStream

object Day9 {

  val dataDirectory = "./data/"
  val fileName = "day9.txt"
  val filePath: String = dataDirectory + fileName
  val fileStream: InputStream = getClass.getResourceAsStream(filePath)

  lazy val inputStream: String = io.Source.fromInputStream(fileStream).mkString

  enum Direction(input: String):
    case Up extends Direction("U")
    case Down extends Direction("D")
    case Left extends Direction("L")
    case Right extends Direction("R")

  object Direction {
    def fromInput(letter: String): Direction = letter match
      case "U" => Up
      case "D" => Down
      case "L" => Left
      case "R" => Right
  }

  case class Motion(direction: Direction, steps: Int)

  case class Position(x: Int, y: Int):
    def move(motion: Motion): Position = motion.direction match
      case Direction.Up => Position(x, y + motion.steps)
      case Direction.Down => Position(x, y - motion.steps)
      case Direction.Right => Position(x + motion.steps, y)
      case Direction.Left => Position(x - motion.steps, y)

    def difference(that: Position): Position =
      Position(that.x - this.x, that.y - this.y)

  def parseSteps(input: String): Seq[Motion] = input.linesIterator.map {
    case s"$dir $steps" => (1 to steps.toInt).map(_ => Motion(Direction.fromInput(dir), 1))
    case row => throw Exception(s"Cannot parse input `$row`.")
  }.toSeq.flatten

  def moveTail(currentTail: Position, nextHead: Position): Position =
    val difference = currentTail.difference(nextHead)
    difference match
      case Position(2, 0) => currentTail.move(Motion(Direction.Right, 1))
      case Position(-2, 0) => currentTail.move(Motion(Direction.Left, 1))
      case Position(0, 2) => currentTail.move(Motion(Direction.Up, 1))
      case Position(0, -2) => currentTail.move(Motion(Direction.Down, 1))
      case Position(2, 1) => Position(currentTail.x + 1, currentTail.y + 1)
      case Position(2, -1) => Position(currentTail.x + 1, currentTail.y - 1)
      case Position(-2, 1) => Position(currentTail.x - 1, currentTail.y + 1)
      case Position(-2, -1) => Position(currentTail.x - 1, currentTail.y - 1)
      case Position(1, 2) => Position(currentTail.x + 1, currentTail.y + 1)
      case Position(-1, 2) => Position(currentTail.x - 1, currentTail.y + 1)
      case Position(1, -2) => Position(currentTail.x + 1, currentTail.y - 1)
      case Position(-1, -2) => Position(currentTail.x - 1, currentTail.y - 1)
      case Position(-2, -2) => Position(currentTail.x - 1, currentTail.y - 1)
      case Position(2, 2) => Position(currentTail.x + 1, currentTail.y + 1)
      case Position(2, -2) => Position(currentTail.x + 1, currentTail.y - 1)
      case Position(-2, 2) => Position(currentTail.x - 1, currentTail.y + 1)
      case _ => currentTail

  def moveNextKnot(headMovement: Seq[Position], startingPoint: Position = Position(0, 0)): Seq[Position] = {
    headMovement.tail.scanLeft(startingPoint)(moveTail)
  }

  @main
  def run9(): Unit = {
    val headMotions = parseSteps(inputStream)
    assert(headMotions.map({ case Motion(_, steps) => steps }).count(_ != 1) == 0)

    val startingPoint = Position(0, 0)
    val headMovement = headMotions.scanLeft(startingPoint)((pos, motion) => pos.move(motion))
    val tailMovement = moveNextKnot(headMovement, startingPoint = startingPoint)
    val solution1 = tailMovement.toSet.size
    println(solution1)

    val allMovements = (1 to 9).scanLeft(headMovement)((x, _) => moveNextKnot(x, startingPoint))
    val solution2 = allMovements.last.toSet.size
    println(solution2)
  }
}
