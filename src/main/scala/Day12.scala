import scala.util.Using
import scala.io.Source

object Day12 {

  val dataDirectory = "./data/"
  val fileName = "day12.txt"
  val filePath: String = dataDirectory + fileName

  def using[T](resourceName: String)(body: Source => T): T = Using.resource(Source.fromResource(resourceName))(body)

  type Grid[T] = Map[Point, T]
  type Graph[V] = Map[Point, Set[V]]

  case class Point(x: Int, y: Int) {
    def up: Point = Point(this.x, this.y + 1)
    def down: Point = Point(this.x, this.y - 1)
    def right: Point = Point(this.x + 1, this.y)
    def left: Point = Point(this.x - 1, this.y)
    def neighbors: Set[Point] = Set(this.up, this.right, this.down, this.left)
  }

  // store graph as adjency list:
  // go through chars and create nodes as Node --> Connected Nodes

  def parseGrid(input: Source): Grid[Char] = {
    for {
      (line, row) <- input.getLines().zipWithIndex
      (char, col) <- line.zipWithIndex
    } yield Point(row, col) -> char
  }.toMap

  def isValidDestination(current: Point, destination: Point, grid: Grid[Char]): Boolean = {
    def elevation(ofChar: Char) = ofChar match
      case 'S' => 'a'
      case 'E' => 'z'
      case c => c
    val res = elevation(grid.get(current)) <= elevation(grid.get(destination)) + 1
  }

  def createAdjacencyList(grid: Grid[Char]): Graph[Point] = {
    grid.foreach(
      (point, _) => point.neighbors.filter(p => isValidDestination(point, p, grid))
    )
  }

  @main
  def run12(): Unit = {
    val grid = using(filePath)(parseGrid)
    println(createAdjacencyList(grid))
  }
}
