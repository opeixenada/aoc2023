import Util.readFile

import scala.annotation.tailrec

@main def day03(): Unit = {

  val input = readFile("resources/day03")

  // Part 1

  val inputIndexed = input.map(_.toIndexedSeq).toIndexedSeq

  val maxX = inputIndexed.head.length
  val maxY = inputIndexed.length

  def adjacentDigitsCoordinates(x: Int, y: Int): Iterable[(Int, Int)] = {
    for {
      i <- Math.max(x - 1, 0) to Math.min(x + 1, maxX)
      j <- Math.max(y - 1, 0) to Math.min(y + 1, maxY)
      if inputIndexed(j)(i).isDigit
    } yield (i, j)
  }

  val symbols: Iterable[Set[(Int, Int)]] = for {
    (line, y) <- inputIndexed.zipWithIndex
    (ch, x) <- line.zipWithIndex
    if !ch.isDigit && !(ch == '.')
  } yield adjacentDigitsCoordinates(x, y).toSet

  val touchedDigits: Set[(Int, Int)] = symbols.flatten.toSet

  @tailrec
  def findNumbers(s: String, i: Int = 0, acc: List[(Int, List[Int])] = Nil): List[(Int, List[Int])] = {
    s.headOption match
      case None => acc

      case Some(ch) if ch.isDigit =>
        val number = s.takeWhile(_.isDigit)
        findNumbers(
          s.drop(number.length + 1),
          i + number.length + 1,
          acc.appended(number.toInt -> (i until i + number.length).toList)
        )

      case _ => findNumbers(s.tail, i + 1, acc)
  }

  val numbers: List[(Int, Set[(Int, Int)])] = for {
    (line, y) <- input.zipWithIndex
    foundNumbers <- findNumbers(line).map { case (n, xs) => n -> xs.map(_ -> y).toSet }
  } yield foundNumbers

  val result1 = numbers.filter { case (n, cs) =>
    cs.intersect(touchedDigits).nonEmpty
  }.map(_._1).sum

  println(result1)

  // Part 2

  val result2 = symbols.foldLeft(0) { case (acc, adjacentDigitsCs) =>
    val adjacentNumbers = numbers.filter { case (n, cs) =>
      cs.intersect(adjacentDigitsCs).nonEmpty
    }.map(_._1)

    if (adjacentNumbers.size != 2) acc else acc + adjacentNumbers.product
  }

  println(result2)
}