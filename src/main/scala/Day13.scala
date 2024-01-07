import PointOfIncidence.*
import Util.{readFile, splitBy}

import scala.annotation.tailrec

object PointOfIncidence {
  @tailrec
  def traverse(numbers: List[(Int, Int)], stack: List[(Int, Int)] = Nil): Option[Int] = {
    (numbers, stack) match
      case (Nil, _) =>
        None

      case ((x, xi) :: xs, (y, yi) :: ys) if x == y && yi == 0 =>
        // Found a match for the first line!
        Some(xi / 2)

      case ((x, xi) :: xs, (y, yi) :: ys) if x == y && xi == yi + 1 =>
        traverse(xs, ys)

      case ((x, _) :: xs, (y, _) :: ys) if x == y =>
        traverse(xs,ys)

      case (x :: xs, _) =>
        traverse(xs, x :: stack)
  }

  private def transpose(xs: List[String]): List[String] = {
    val indexedSeq = xs.map(_.toIndexedSeq).toIndexedSeq

    indexedSeq.head.indices.map { x =>
      indexedSeq.indices.map { y =>
        indexedSeq(y)(x)
      }.mkString
    }.toList
  }

  private def toNumbers(xs: List[String]): List[Int] = {
    xs.map { string =>
      Integer.parseInt(string.replace('#', '1').replace('.', '0'), 2)
    }
  }

  case class Axis(rows: Set[Int] = Set.empty, cols: Set[Int] = Set.empty)

  def getAxisScore(xs: List[String]): Option[Int] = {
    val axis = getAxis(xs)
    toScore(axis)
  }

  private def getAxis(xs: List[String]): Axis = {
    val axis = List(xs, transpose(xs))
      .map(toNumbers)
      .map { ns =>
        val down = traverse(ns.zipWithIndex)
        val up = traverse(ns.reverse.zipWithIndex)
        List(down.map(_ + 1), up.map(ns.length - 1 - _)).flatten.toSet
      }

    Axis(rows = axis.head, cols = axis.tail.head)
  }

  private def flip(ch: Char): Char = if (ch == '#') '.' else '#'

  private def variants(xs: List[String]): Iterable[List[String]] = {
    for {
      x <- xs.indices
      y <- xs.head.indices
    } yield {
      xs.take(x) ++ (flipSymbol(xs.drop(x).head, y) :: xs.drop(x + 1))
    }
  }

  private def flipSymbol(s: String, i: Int): String = {
    s.take(i) + flip(s.drop(i).head) + s.drop(i + 1)
  }

  def getAlternativeAxisScore(xs: List[String]): Option[Int] = {
    val original = getAxis(xs)

    val r = variants(xs)
      .map(getAxis)
      .map { x =>
        Axis(x.rows.diff(original.rows), x.cols.diff(original.cols))
      }
      .find { x =>
        x.rows.nonEmpty || x.cols.nonEmpty
      }
      .getOrElse(Axis())

    toScore(r)
  }

  private def toScore(axis: Axis): Option[Int] = List(axis.rows, axis.cols)
    .zip(List(100, 1))
    .flatMap { case (maybeIndex, coefficient) =>
      maybeIndex.map(_ * coefficient)
    }
    .headOption
}

@main def day13(): Unit = {

  val input = splitBy(readFile("resources/day13"), { (x: String) => x.isBlank })

  // Part 1

  val result1 = input.flatMap(getAxisScore).sum

  println(result1)

  // Part 2

  val result2 = input.flatMap(getAlternativeAxisScore).sum

  println(result2)
}