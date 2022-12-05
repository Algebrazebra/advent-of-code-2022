import scala.io.Source
import java.io.InputStream

object Day5 {

  val dataDirectory = "./data/"
  val fileName = "day5.txt"
  val filePath: String = dataDirectory + fileName
  val fileStream: InputStream = getClass.getResourceAsStream(filePath)

  lazy val inputStream: String = io.Source.fromInputStream(fileStream).mkString

  type Crate = Char
  type Stack = List[Char]
  type Crates = Seq[Stack]

  case class CraneInstruction(numberOfCrates: Int, moveFrom: Int, moveTo: Int)
  type InstructionSet = Seq[CraneInstruction]

  def parseInput(input: String): (Crates, InstructionSet) = {
    val Array(crates, instructions) = input.split("\n\n", 2)
    (parseCrates(crates), parseCraneInstructions(instructions))
  }

  def parseCraneInstructions(input: String): InstructionSet = {
    input.split("\n").map({
      instruction => instruction match
        case s"move $crates from $pos1 to $pos2" => CraneInstruction(crates.toInt, pos1.toInt - 1, pos2.toInt - 1)
        case _ => throw Exception("Something went wrong parsing instructions.")
    }).toSeq
  }

  def parseCrates(input: String): Crates = {
    // Read lines of input create configuration string and remove
    // the last line that enumerates the create stack positions.
    // Then the transpose function turns the rows into columns.
    val lines = input.linesIterator.toSeq
    val columns = lines.reverse.tail.transpose

    // Select every fourth column starting from the first column,
    // because these are the columns containing the letter identifying
    // the crates. Reverse so that the column `List` serves as LIFO queue.
    (1 until columns.size by 4)
      .map(idx => columns(idx).filter(_ != ' ').reverse.toList)
  }

  def executeInstructionPart1(crates: Crates, instruction: CraneInstruction): Crates = {
    val CraneInstruction(count, from, to) = instruction
    val (cratesToMove, remainingCrates) = crates(from).splitAt(count)
    crates
      .updated(from, remainingCrates)
      .updated(to, cratesToMove.reverse ++ crates(to))
  }

  def executeInstructionPart2(crates: Crates, instruction: CraneInstruction): Crates = {
    val CraneInstruction(count, from, to) = instruction
    val (cratesToMove, remainingCrates) = crates(from).splitAt(count)
    crates
      .updated(from, remainingCrates)
      .updated(to, cratesToMove ++ crates(to))
  }

  def topCrates(crates: Crates): String = crates.map(_.head).mkString

  def printCrates(crates: Crates): Unit = crates.foreach(println)


  @main
  def run5(): Unit = {
    val (crates, instructions) = parseInput(inputStream)

    val finalStatePart1 = instructions.foldLeft(crates)(executeInstructionPart1)
    val solutionPart1 = topCrates(finalStatePart1)
    println(solutionPart1)

    val finalStatePart2 = instructions.foldLeft(crates)(executeInstructionPart2)
    val solutionPart2 = topCrates(finalStatePart2)
    println(solutionPart2)
  }

}
