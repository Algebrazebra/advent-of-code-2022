import Day7.CommandOutput.Directory

import scala.io.Source
import scala.collection.mutable.Map
import java.io.InputStream
import scala.annotation.tailrec
import scala.collection.mutable

object Day7 {

  val dataDirectory = "./data/"
  val fileName = "day7.txt"
  val filePath: String = dataDirectory + fileName
  val fileStream: InputStream = getClass.getResourceAsStream(filePath)

  lazy val inputStream: String = io.Source.fromInputStream(fileStream).mkString

  enum Command:
    case ChangeDirectory(directory: String)
    case ListFiles

  enum CommandOutput:
    case Cmd(cmd: Command)
    case Directory(name: String)
    case File(size: Int, name: String)

  class DirectoryStructure(
                            val name: String,
                            val files: mutable.Map[String, Int],
                            val parentDirectory: DirectoryStructure | Null,
                            val childDirectories: mutable.Map[String, DirectoryStructure],
                          )

  def parseInput(input: String): Seq[CommandOutput] = {
    import Command.*
    import CommandOutput.*
    input.linesIterator.map {
      case s"$$ cd $directory" => Cmd(ChangeDirectory(directory))
      case s"$$ ls" => Cmd(ListFiles)
      case s"dir $directory" => Directory(directory)
      case s"$size $file" => File(size.toInt, file)
    }.toSeq
  }

  def execute(commands: Seq[CommandOutput], rootDirectory: DirectoryStructure): DirectoryStructure = {

    import Command.*
    import CommandOutput.*

    @tailrec
    def loop(remainingCommands: Seq[CommandOutput], currentDirectory: DirectoryStructure | Null): DirectoryStructure = remainingCommands match {
      case Cmd(ChangeDirectory("..")) :: tail => loop(tail, currentDirectory.parentDirectory)
      case Cmd(ChangeDirectory("/")) :: tail => loop(tail, rootDirectory)
      case Cmd(ChangeDirectory(dirName)) :: tail => loop(tail, currentDirectory.childDirectories(dirName))
      case Cmd(ListFiles) :: tail => loop(tail, currentDirectory)
      case File(size, name) :: tail =>
        currentDirectory.files.put(name, size)
        loop(tail, currentDirectory)
      case Directory(name) :: tail =>
        currentDirectory.childDirectories.put(name, DirectoryStructure(name, mutable.Map.empty, currentDirectory, mutable.Map.empty))
        loop(tail, currentDirectory)
      case Nil => rootDirectory
    }
    loop(commands, rootDirectory)
  }

  def calculateDirectorySize(dir: DirectoryStructure): Int =
    dir.files.values.sum + dir.childDirectories.values.map(calculateDirectorySize).sum

  def calculateConditionalTotalSize(dir: DirectoryStructure, predicate: Int => Boolean): Seq[Int] =
    val dirSize = calculateDirectorySize(dir)
    val children = dir.childDirectories.values.flatMap(calculateConditionalTotalSize(_, predicate))
    if predicate(dirSize) then dirSize +: children.toSeq else children.toSeq

  @main
  def run7(): Unit = {
    val commands = parseInput(inputStream)
    val emptyRoot = DirectoryStructure("/", mutable.Map.empty, null, mutable.Map.empty)
    val rootDir = execute(commands, emptyRoot)

    val solution1 = calculateConditionalTotalSize(rootDir, _ < 100000).sum
    println(solution1)

    val totalUsed = calculateDirectorySize(rootDir)
    val totalUnused = 70_000_000 - totalUsed
    val required = 30_000_000 - totalUnused
    val solution2 = calculateConditionalTotalSize(rootDir, _ >= required).min
    println(solution2)

  }

}
