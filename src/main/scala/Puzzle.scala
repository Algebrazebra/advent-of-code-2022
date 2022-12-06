import java.io.InputStream
import scala.io.Source

trait Puzzle {

  def getInputStream(day: Int): String = {
    val fileName = s"day$day.txt"
    val dataDirectory = "./data/"
    val filePath = dataDirectory + fileName
    lazy val fileStream = getClass.getResourceAsStream(filePath)
    val inputStream = io.Source.fromInputStream(fileStream).mkString.trim
    inputStream
  }

}
