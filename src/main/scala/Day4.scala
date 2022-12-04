import java.io.InputStream
import scala.io.Source

object Day4 {

  val dataDirectory = "./data/"
  val fileName = "day4.txt"
  val filePath: String = dataDirectory + fileName
  val fileStream: InputStream = getClass.getResourceAsStream(filePath)

  lazy val inputStream: String = io.Source.fromInputStream(fileStream).mkString.trim

  case class Assignment(startSection: Int, endSection: Int) {

    def doesFullyContain(otherAssignment: Assignment): Boolean = otherAssignment match
      case that if this.startSection <= that.startSection && this.endSection >= that.endSection => true
      case _ => false

    def doesOverlap(otherAssignment: Assignment): Boolean = {
      val thisInterval = (this.startSection to this.endSection).toSet
      val thatInterval = (otherAssignment.startSection to otherAssignment.endSection).toSet
      (thisInterval intersect thatInterval).nonEmpty
    }
  }
  object Assignment {
    def apply(assignmentString: String): Assignment = {
      val interval = assignmentString.split("-", 2).toSeq.take(2)
      Assignment(interval.head.toInt, interval(1).toInt)
    }
  }

  def parseInput(input: String): Seq[(Assignment, Assignment)] = {
    val parsedInput = input
      .split("\n")
      .map(_.split(",").toSeq)
      .toIndexedSeq
    val parsedAssignments = parsedInput
      .map({
        pairs => (Assignment(pairs(0)), Assignment(pairs(1)))
      })
    parsedAssignments
  }


  @main
  def run4(): Unit = {
    val parsedInput = parseInput(inputStream)
    val solution1 = parsedInput
      .map({assignmentPair =>
        assignmentPair._1.doesFullyContain(assignmentPair._2)
        || assignmentPair._2.doesFullyContain(assignmentPair._1)
      })
      .map(x => if(x) 1 else 0)
      .sum
    println(solution1)
    val solution2 = parsedInput
      .map({assignmentPair =>
        assignmentPair._1.doesOverlap(assignmentPair._2)
      })
      .map(x => if(x) 1 else 0)
      .sum
    println(solution2)
  }
}

