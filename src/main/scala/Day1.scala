import java.io.InputStream
import scala.io.Source

object Day1 extends Puzzle {

  def partitionByElves(input: String): Seq[String] = input.split("\n\n").toSeq

  def parseItems(items: String): Seq[Int] = items.linesIterator.map(_.toInt).toSeq

  def parsePartitions(partitions: Seq[String]): Seq[Seq[Int]] = partitions.map(parseItems)

  def sumCalories(parsedInput: Seq[Seq[Int]]): Seq[Int] = parsedInput.map(_.sum)

  @main
  def run(): Unit = {
    val parsedInput = parsePartitions(partitionByElves(getInputStream(1)))
    val calories = sumCalories(parsedInput).sortWith(_ > _)
    val mostCalories = calories.max
    println(mostCalories)
    val caloriesOfTopThreeElves = calories.take(3).sum
    println(caloriesOfTopThreeElves)
  }
}