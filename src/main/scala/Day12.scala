import HotSprings.*
import Util.readFile

import scala.annotation.tailrec

object HotSprings {
  def countArrangements(s: String, ns: List[Int]): Int = exploreArrangements(ns, List(s))

  private def matches(s: String, ns: List[Int]): Boolean = {
    "#+".r.findAllMatchIn(s).map(_.matched.length).toList == ns
  }

  @tailrec
  private def exploreArrangements(ns: List[Int], stack: List[String], count: Int = 0): Int = {
    stack match
      case Nil => count
      case x :: xs if !x.contains('?') && matches(x, ns) => exploreArrangements(ns, xs, count + 1)
      case x :: xs if !x.contains('?') => exploreArrangements(ns, xs, count)
      case x :: xs =>
        val i = x.indexWhere(_ == '?')
        val newStack = List('.', '#').map { ch =>
          s"${x.substring(0, i)}$ch${x.drop(i + 1)}"
        } ++ xs
        exploreArrangements(ns, newStack, count)
  }
}

@main def day12(): Unit = {

  val input = readFile("resources/day12").map { s =>
    val xs = s.split(' ')
    val numbers = xs.tail.head.split(',').map(_.toInt).toList
    xs.head -> numbers
  }

  // Part 1

  val result1 = input.map(countArrangements).sum

  println(result1)

  // Part 2

  val result2 = "foo"

  println(result2)
}