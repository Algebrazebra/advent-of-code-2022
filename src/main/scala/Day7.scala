import scala.io.Source
import java.io.InputStream

object Day7 {

  val dataDirectory = "./data/"
  val fileName = "day7.txt"
  val filePath: String = dataDirectory + fileName
  val fileStream: InputStream = getClass.getResourceAsStream(filePath)

  lazy val inputStream: String = io.Source.fromInputStream(fileStream).mkString



  @main
  def run7(): Unit = {
    ???
  }

}
