import java.io.InputStream
import scala.io.Source
import scala.util.Using

object Day1 {

  val dataDirectory = "./data/"
  val fileName = "day1a.txt"
  val filePath: String = dataDirectory + fileName
  val fileStream: InputStream = getClass.getResourceAsStream(filePath)

  lazy val inputStream: String = io.Source.fromInputStream(fileStream).mkString.trim

  def partitionByElves(input: String): Seq[String] = input.split("\n\n").toSeq

  def parseItems(items: String): Seq[Int] = items.linesIterator.map(_.toInt).toSeq

  def parsePartitions(partitions: Seq[String]): Seq[Seq[Int]] = partitions.map(parseItems)

  def sumCalories(parsedInput: Seq[Seq[Int]]): Seq[Int] = parsedInput.map(_.sum)

  @main
  def run(): Unit = {
    val parsedInput = parsePartitions(partitionByElves(inputStream))
    val calories = sumCalories(parsedInput).sortWith(_ > _)
    val mostCalories = calories.max
    println(mostCalories)
    val caloriesOfTopThreeElves = calories.take(3).sum
    println(caloriesOfTopThreeElves)
  }
}