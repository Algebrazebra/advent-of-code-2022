import java.io.InputStream
import scala.io.Source


object Day10 {

  val dataDirectory = "./data/"
  val fileName = "day10.txt"
  val filePath: String = dataDirectory + fileName
  val fileStream: InputStream = getClass.getResourceAsStream(filePath)

  lazy val inputStream: String = io.Source.fromInputStream(fileStream).mkString

  enum Cmd:
    case Noop
    case Addx(x: Int)

  def parseInput(input: String): Seq[Cmd] = input.linesIterator.map {
    case "noop" => Cmd.Noop
    case s"addx $x" => Cmd.Addx(x.toInt)
    case e => throw Exception(s"Parsing failed on: $e")
  }.toSeq

  def execute(commands: Seq[Cmd]): Seq[Int] = {
    val initState = Seq(1)
    // Executing a command might create one (noop) or two (addx) cycles.
    // Iterating through the Seq with scanLeft calculates the register state
    // in each cycle.
    commands.scanLeft(initState) { (values, cmd) =>
      cmd match
        case Cmd.Noop => values.last :: Nil
        case Cmd.Addx(x) => values.last :: values.last + x :: Nil
    // Flattening the output, creates the Seq[Int] in which each element
    // represents the register state for each cycle in order.
    }.flatten
  }

  def calculateSignalStrength(registerStates: Seq[Int]): Int = {
    if registerStates.size < 20 then 0
    else registerStates
      .zipWithIndex                                 // add cycle index explicitly
      .map((state, cycle) => (state, cycle + 1))    // index should start with 1
      .filter((_, cycle) => (cycle - 20) % 40 == 0) // only take 20th, 60th, 100th element
      .take(6)                                      // only takes up until the 220th element
      .map((state, cycle) => state * cycle)         // calculate signal strength at position
      .sum
  }

  def drawCRT(registerStates: Seq[Int], width: Int): Seq[Char] = {
    def drawSpritePredicate(reg: Int, pos: Int): Boolean = (reg - (pos % width)).abs <= 1
    registerStates
      .zipWithIndex
      .map {
        case (reg, pos) if drawSpritePredicate(reg, pos) => '#'
        case _ => '.'
      }
      .grouped(width)
      .map(_.mkString)
      .mkString("\n")
  }

  @main
  def run10(): Unit = {
    val commands = parseInput(inputStream)
    val registerStates = execute(commands)

    val solution1 = calculateSignalStrength(registerStates)
    println(solution1)

    val solution2 = drawCRT(registerStates, 40)
    println(solution2)
  }
}
