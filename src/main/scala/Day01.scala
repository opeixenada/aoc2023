import Util.readFile
import Util.digitToInt

@main def day01(): Unit = {

  val input = readFile("resources/day01")

  // Part 1
  val result1 = input.foldLeft(List.empty[Int]) { case (acc, line) =>
    val a = digitToInt(line.find(_.isDigit).get) * 10
    val b = digitToInt(line.findLast(_.isDigit).get)
    acc.::(a + b)
  }.sum

  println(result1)

  // Part 2

  val numbers = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
  )
  
  val pattern = s"(${numbers.keys.mkString("|")}|\\d)".r
  val patternReversed = s"(${numbers.keys.map(_.reverse).mkString("|")}|\\d)".r

  def toInt(s: String) = if (s.head.isDigit) digitToInt(s.head) else numbers(s)

  val result2 = input.foldLeft(List.empty[Int]) { case (acc, line) =>
    val a = pattern.findFirstIn(line).map(toInt).get * 10
    val b = patternReversed.findFirstIn(line.reverse).map(_.reverse).map(toInt).get
    acc.::(a + b)
  }.sum

  println(result2)
}