import MirageMaintenance.*
import Util.readFile

import scala.annotation.tailrec

object MirageMaintenance {
  def extrapolateForwards(xs: List[Int]): Int = findNextValue(generateSequences(List(xs)))

  def extrapolateBackwards(xs: List[Int]): Int = findPreviousValue(generateSequences(List(xs)))

  @tailrec
  private def generateSequences(xss: List[List[Int]]): List[List[Int]] = xss match
    case xs :: tail if xs.forall(_ == 0) => xss
    case xs :: tail => generateSequences(xs.zip(xs.tail).map((a, b) => b - a) :: xss)

  @tailrec
  private def findNextValue(xss: List[List[Int]]): Int = xss match
    case xs :: Nil => xs.last
    case xs :: ys :: tail => findNextValue(ys.appended(xs.last + ys.last) :: tail)

  @tailrec
  private def findPreviousValue(xss: List[List[Int]]): Int = xss match
    case xs :: Nil => xs.head
    case xs :: ys :: tail => findPreviousValue(((ys.head - xs.head) :: ys) :: tail)
}

@main def day09(): Unit = {

  val input = readFile("resources/day09").map { s =>
    s.split(' ').map(_.toInt).toList
  }

  // Part 1

  val result1 = input.map(extrapolateForwards).sum

  println(result1)

  // Part 2

  val result2 = input.map(extrapolateBackwards).sum

  println(result2)
}