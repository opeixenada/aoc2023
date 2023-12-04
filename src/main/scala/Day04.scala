import Util.readFile

import scala.annotation.tailrec

@main def day04(): Unit = {

  val input = readFile("resources/day04")

  // Part 1

  val cardNumberRegex = "Card\\s+(\\d+): ".r

  @tailrec
  def parse(s: String, xs: List[Int] = Nil): List[Int] = {
    if (s.isEmpty) xs
    else parse(s.drop(3), s.take(2).trim.toInt :: xs)
  }

  val cards: List[(Int, Set[Int], Set[Int])] = input.map { line =>
    val cardNumber = cardNumberRegex.findFirstMatchIn(line).map(_.group(1).toInt).get

    val numbers = line
      .substring(line.indexOf(':') + 2)
      .split(" \\| ")
      .map { s => parse(s).toSet }

    (cardNumber, numbers.head, numbers.last)
  }

  val result1 = cards.foldLeft(0) { case (acc, (_, xs, ys)) =>
    acc + (if (xs.intersect(ys).nonEmpty) Math.pow(2, xs.intersect(ys).size - 1) else 0).toInt
  }

  println(result1)

  // Part 2

  @tailrec
  def runCards(cards: List[(Int, Set[Int], Set[Int])], counts: Map[Int, Int]): Map[Int, Int] = {
    cards match
      case Nil => counts
      case x::xs =>
        val winning = x._2.intersect(x._3).size
        if (winning == 0) runCards(xs, counts)
        else {
          val factor = counts(x._1)

          val newCounts = ((x._1 + 1) to (x._1 + winning)).foldLeft(counts) { (acc, n) =>
            acc ++ acc.get(n).map { count => n -> (count + factor) }.toMap
          }

          runCards(xs, newCounts)
        }
  }

  val result2 = runCards(cards, cards.map(_._1 -> 1).toMap).values.sum

  println(result2)
}