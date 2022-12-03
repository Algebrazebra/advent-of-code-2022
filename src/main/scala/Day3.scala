import java.io.InputStream
import scala.io.Source

object Day3 {

  val dataDirectory = "./data/"
  val fileName = "day3.txt"
  val filePath: String = dataDirectory + fileName
  val fileStream: InputStream = getClass.getResourceAsStream(filePath)

  lazy val inputStream: String = io.Source.fromInputStream(fileStream).mkString.trim

  case class Rucksack(leftCompartment: String, rightCompartment: String) {
    def findCommonItem: Char = {
      val commonChars = leftCompartment.toSet & rightCompartment.toSet
      commonChars.head
    }
  }

  case class ElfGroup(first: String, second: String, third: String) {
    def findCommonItem: Char = {
      val commonChars = first.toSet & second.toSet & third.toSet
      commonChars.head
    }
  }

  def parseInput(input: String): Seq[Rucksack] = {
    input
      .split("\n")
      .map(x => x.splitAt(x.length/2))
      .map({
        (left, right) => Rucksack(left, right)
      })
      .toSeq
  }

  def parseInputPart2(input: String): Seq[ElfGroup] = {
    input
      .split("\n")
      .grouped(3)
      .map(x => ElfGroup(x(0), x(1), x(2)))
      .toSeq
  }

  val priorities: Map[Char, Int] = {
    val characters = ('a' to 'z').toList ++ ('A' to 'Z').toList
    val priorities = (1 to 26*2).toList
    (characters zip priorities).toMap[Char, Int]
  }

  @main
  def run3(): Unit = {
    val solution1 = parseInput(inputStream)
      .map(i => i.findCommonItem)
      .map(i => priorities(i))
      .sum
    println(solution1)

    val solution2 = parseInputPart2(inputStream)
      .map(i => i.findCommonItem)
      .map(i => priorities(i))
      .sum
    println(solution2)
  }
}
