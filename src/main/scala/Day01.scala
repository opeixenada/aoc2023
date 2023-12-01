import Util.readFile

import scala.collection.immutable.Seq

@main def day01(): Unit = {

  val input = readFile("resources/day01")

  // Part 1

  val result1 = input
    .foldLeft(List.empty[Int]) { case (acc, line) =>
      acc.::(Seq(line.find(_.isDigit), line.findLast(_.isDigit)).flatten.mkString.toInt)
    }
    .sum

  println(result1)

  // Part 2

  val numbers = Map(
    "one" -> '1',
    "two" -> '2',
    "three" -> '3',
    "four" -> '4',
    "five" -> '5',
    "six" -> '6',
    "seven" -> '7',
    "eight" -> '8',
    "nine" -> '9'
  )

  val pattern = s"(${numbers.keys.mkString("|")}|\\d)".r
  val patternReversed = s"(${numbers.keys.map(_.reverse).mkString("|")}|\\d)".r

  val result2 = input
    .foldLeft(List.empty[Int]) { case (acc, line) =>
      acc.::(
        Seq(pattern.findFirstIn(line), patternReversed.findFirstIn(line.reverse).map(_.reverse)).flatten
          .map { x => numbers.getOrElse(x, x) }
          .mkString
          .toInt
      )
    }
    .sum

  println(result2)
}