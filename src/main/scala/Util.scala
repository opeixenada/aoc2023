import scala.io.Source

object Util {
  def readFile(filename: String): List[String] = {
    val bufferedSource = Source.fromFile(filename)
    val result = bufferedSource.getLines.toList
    bufferedSource.close
    result
  }
  
  def digitToInt(char: Char): Int = char - '0'

  def withTimeLogging[A](block: => A): A = {
    val startTime = System.currentTimeMillis()
    val result = block
    val endTime = System.currentTimeMillis()
    val executionTime = endTime - startTime
    println(s"Execution Time: $executionTime ms")
    result
  }
}
