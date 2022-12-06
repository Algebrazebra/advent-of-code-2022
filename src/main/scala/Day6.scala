import scala.io.Source
import java.io.InputStream
import javax.print.attribute.standard.JobKOctetsProcessed

object Day6 {

  val dataDirectory = "./data/"
  val fileName = "day6.txt"
  val filePath: String = dataDirectory + fileName
  val fileStream: InputStream = getClass.getResourceAsStream(filePath)

  lazy val inputStream: String = io.Source.fromInputStream(fileStream).mkString

  def process(input: String, lenDistinct: Int): Int =
    input.sliding(lenDistinct).takeWhile(w => w.chars().distinct().count() != lenDistinct).size + lenDistinct


  @main
  def run6(): Unit = {
    val solution1 = process(inputStream, 4)
    println(solution1)

    val solution2 = process(inputStream, 14)
    println(solution2)
  }

}
