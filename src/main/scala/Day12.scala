import java.io.InputStream
import scala.io.Source
import scala.math.{cos, pow, sqrt}

object Day12 {

  val dataDirectory = "./data/"
  val fileName = "day12.txt"
  val filePath: String = dataDirectory + fileName
  val fileStream: InputStream = getClass.getResourceAsStream(filePath)

  lazy val inputStream: String = io.Source.fromInputStream(fileStream).mkString

  type Grid[T] = Seq[Seq[T]]

  sealed abstract class Graph
  case class Vertex() extends Graph
  case class Edge(v1: Vertex, v2: Vertex) extends Graph

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(line => line.toList).toSeq

  def dijkstra(graph: Grid[Char], source): (Seq[Int], Seq)

  @main
  def run12(): Unit = {
    val grid = parseGrid("Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi")
    println(grid)
  }
}
