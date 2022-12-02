import scala.io.Source
import java.io.InputStream

sealed trait Play {
  case object Rock extends Play
  case object Paper extends Play
  case object Scissors extends Play
}


object Day2 {

  val dataDirectory = "./data/"
  val fileName = "day2.txt"
  val filePath: String = dataDirectory + fileName
  val fileStream: InputStream = getClass.getResourceAsStream(filePath)

  lazy val inputStream: String = io.Source.fromInputStream(fileStream).mkString.trim

  def partitionByRounds(input: String): Seq[String] = input.split("\n").toSeq

  def parseRounds(round: String): (String, String) = {
    round.split(" ").toSeq.take(2) match { case Seq(l, r) => Tuple2(l, r) }
  }

  def calcPointsForPlay(round: (String, String)): Int = round match {
    case (_, "X") => 1
    case (_, "Y") => 2
    case (_, "Z") => 3
  }

  def calcPointsForOutcomePart2(round: (String, String)): Int = round match {
    case (_, "X") => 0
    case (_, "Y") => 3
    case (_, "Z") => 6
  }

  def calcPointsForOutcome(round: (String, String)): Int = round match {

    case ("A", "X") => 3  // Rock vs. Rock
    case ("A", "Y") => 6  // Rock vs. Paper
    case ("A", "Z") => 0  // Rock vs. Scissors

    case ("B", "X") => 0  // Paper vs. Rock
    case ("B", "Y") => 3  // Paper vs. Paper
    case ("B", "Z") => 6  // Paper vs. Scissors

    case ("C", "X") => 6  // Scissors vs. Rock
    case ("C", "Y") => 0  // Scissors vs. Paper
    case ("C", "Z") => 3  // Scissors vs. Scissors
  }

  def calcPointsForPlayPart2(round: (String, String)): Int = round match {
    case ("A", "X") => 3  // Rock -> Loose --> C
    case ("A", "Y") => 1 // Rock -> Draw --> A
    case ("A", "Z") => 2 // Rock --> Win --> B

    case ("B", "X") => 1  // Loose: A
    case ("B", "Y") => 2  // Draw: B
    case ("B", "Z") => 3  // Win: C

    case ("C", "X") => 2  // Loose: B
    case ("C", "Y") => 3  // Draw: C
    case ("C", "Z") => 1  // Win: A
  }


  @main
  def run2(): Unit = {
    val partitionedRounds = partitionByRounds(inputStream)
    val parsedRounds = partitionedRounds.map(parseRounds)
    val pointsForPlay: Int = parsedRounds.map(round => calcPointsForPlay(round)).sum
    val pointsForOutcome: Int = parsedRounds.map(round => calcPointsForOutcome(round)).sum
    val totalPoints = pointsForPlay + pointsForOutcome
    println(totalPoints)

    val pointsForPlayPart2 = parsedRounds.map(round => calcPointsForPlayPart2(round)).sum
    val pointsForOutcomePart2 = parsedRounds.map(round => calcPointsForOutcomePart2(round)).sum
    val totalPointsPart2 = pointsForPlayPart2 + pointsForOutcomePart2
    println(totalPointsPart2)

  }
}
