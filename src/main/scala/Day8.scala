import java.io.InputStream
import scala.annotation.tailrec
import scala.io.Source

object Day8 {

  val dataDirectory = "./data/"
  val fileName = "day8.txt"
  val filePath: String = dataDirectory + fileName
  val fileStream: InputStream = getClass.getResourceAsStream(filePath)

  lazy val inputStream: String = io.Source.fromInputStream(fileStream).mkString

  def parse(input: String): Seq[Seq[Int]] = {
    input.linesIterator.map(line =>
      line.toVector.map(char =>
        char.asDigit)
    ).toVector
  }

  def processVisibilityLeft(line: Seq[Int]): Seq[Boolean] = {
    @tailrec
    def process(highestSoFar: Int, remainingLine: Seq[Int], isVisible: Seq[Boolean]): Seq[Boolean] = {
      remainingLine match
        case head +: tail if head > highestSoFar => process(head, tail, isVisible :+ true)
        case head +: tail if head <= highestSoFar => process(highestSoFar, tail, isVisible :+ false)
        case Seq() => isVisible
    }
    process(-1, line, Seq.empty)
  }

  def processViewingDistanceLeft(line: Seq[Int]): Seq[Int] = {
    def calculateDistance(reversePrevious: Seq[Int], elem: Int): Int = {
      @tailrec
      def loop(xs: Seq[Int], elem: Int, distance: Int): Int = xs match {
        case head +: tail if head < elem => loop(tail, elem, distance + 1)
        case head +: _ if head >= elem => distance + 1
        case Seq() => distance
      }
      loop(reversePrevious, elem, 0)
    }
    @tailrec
    def process(reversePrevious: Seq[Int], remainingLine: Seq[Int], distances: Seq[Int]): Seq[Int] = {
      remainingLine match
        case head +: tail => process(head +: reversePrevious, tail, distances :+ calculateDistance(reversePrevious, head))
        case Seq() => distances
    }
    process(Seq.empty, line, Seq.empty)
  }

  def processViewingDistanceRight(line: Seq[Int]): Seq[Int] = processViewingDistanceLeft(line.reverse).reverse

  def processViewingDistanceLine(line: Seq[Int]): Seq[Int] = {
    processViewingDistanceLeft(line).zip(processViewingDistanceRight(line)).map(_ * _)
  }

  def processRows2(grid: Seq[Seq[Int]]): Seq[Seq[Int]] = grid.map(processViewingDistanceLine)

  def processColumns2(grid: Seq[Seq[Int]]): Seq[Seq[Int]] = processRows2(grid.transpose).transpose

  def processGrid2(grid: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val rowVisibility = processRows2(grid)
    val colVisibility = processColumns2(grid)

    rowVisibility zip colVisibility map { row => row._1 zip row._2 map { _ * _ } }
  }


  def processVisibilityRight(line: Seq[Int]): Seq[Boolean] = processVisibilityLeft(line.reverse).reverse

  def processVisibilityLine(line: Seq[Int]): Seq[Boolean] = {
    processVisibilityLeft(line).zip(processVisibilityRight(line)).map(_ | _)
  }

  def processRows(grid: Seq[Seq[Int]]): Seq[Seq[Boolean]] = grid.map(processVisibilityLine)

  def processColumns(grid: Seq[Seq[Int]]): Seq[Seq[Boolean]] = processRows(grid.transpose).transpose

  def processGrid(grid: Seq[Seq[Int]]): Seq[Seq[Boolean]] = {
    val rowVisibility = processRows(grid)
    val colVisibility = processColumns(grid)

    rowVisibility zip colVisibility map { row => row._1 zip row._2 map { _ | _ } }
  }

  @main
  def run8(): Unit = {
    val parsedInput = parse(inputStream)

    val visibilityGrid = processGrid(parsedInput)
    val solution1 = visibilityGrid.map(row => row.count(_ == true)).sum
    println(solution1)

    val viewingDistanceGrid = processGrid2(parsedInput)
    val solution2 = viewingDistanceGrid.flatten.max
    println(solution2)
  }

}
