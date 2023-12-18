import PipeMaze.*
import Util.readFile

import scala.annotation.tailrec
import scala.collection.immutable.Set

object PipeMaze {

  private def invert(direction: Char): Char = direction match
    case '>' => '<'
    case '^' => 'v'
    case 'v' => '^'
    case _ => '>'

  private def getDirections(ch: Char): List[Char] = ch match
    case '|' => List('^', 'v')
    case '-' => List('<', '>')
    case 'L' => List('^', '>')
    case 'J' => List('^', '<')
    case '7' => List('<', 'v')
    case 'F' => List('v', '>')
    case _ => Nil

  private val allDirections: List[Char] = List('^', '>', 'v', '<')

  def findLoopHalfLength(input: IndexedSeq[IndexedSeq[Char]]): Int = {
    findLoop(input)._1.size / 2
  }

  private def findLoop(input: IndexedSeq[IndexedSeq[Char]]): (List[(Int, Int)], Char) = {

    val sX: Int = input.indexWhere(_.contains('S'))
    val sY: Int = input(sX).indexWhere(_ == 'S')

    def getSymbol(x: Int, y: Int): Option[Char] = {
      if (x < 0 || x >= input.length || y < 0 || y >= input.head.length) None
      else Some(input(x)(y))
    }

    def findConnectedInDirection(direction: Char, x: Int, y: Int): Option[(Int, Int)] = {
      direction match
        case '^' =>
          getSymbol(x - 1, y) match
            case Some(ch) if Set('|', '7', 'F').contains(ch) => Some((x - 1, y))
            case _ => None

        case '>' =>
          getSymbol(x, y + 1) match
            case Some(ch) if Set('-', 'J', '7').contains(ch) => Some(x, y + 1)
            case _ => None

        case 'v' =>
          getSymbol(x + 1, y) match
            case Some(ch) if Set('|', 'L', 'J').contains(ch) => Some(x + 1, y)
            case _ => None

        case _ => // <
          getSymbol(x, y - 1) match
            case Some(ch) if Set('-', 'L', 'F').contains(ch) => Some(x, y - 1)
            case _ => None
    }

    def findConnected(directions: List[Char], x: Int, y: Int) = directions.flatMap { direction =>
      findConnectedInDirection(direction, x, y).map { cs => direction -> cs }
    }

    val startingPoints: List[(Char, (Int, Int))] = findConnected(allDirections, sX, sY)

    val realS = startingPoints.map(_._1).toSet match
      case xs if xs == Set('^', 'v') => '|'
      case xs if xs == Set('<', '>') => '-'
      case xs if xs == Set('^', '>') => 'L'
      case xs if xs == Set('^', '<') => 'J'
      case xs if xs == Set('<', 'v') => '7'
      case xs if xs == Set('v', '>') => 'F'

    @tailrec
    def traverse(state: List[(Char, (Int, Int))], traversed: List[(Int, Int)] = Nil): List[(Int, Int)] = {
      if (state.map(_._2).exists(traversed.contains(_))) (traversed ++ state.map(_._2)).distinct
      else {
        val nextState = state.flatMap { case (direction, (x, y)) =>
          findConnected(getDirections(input(x)(y)).filterNot(_ == invert(direction)), x, y).headOption
        }
        traverse(nextState, traversed ++ state.map(_._2))
      }
    }

    (traverse(startingPoints, traversed = List(sX -> sY)), realS)
  }

  def countInsideDots(input: IndexedSeq[IndexedSeq[Char]]): Int = {

    val (loop, realS) = findLoop(input)

    def countWallsToTheRight(x: Int, y: Int): Int = {
      val chars = (y until input.head.length).map { y =>
        if (loop.contains(x, y)) input(x)(y)
        else '.'
      }.map { x => if (x == 'S') realS else x }.toList

      countWalls(chars)
    }

    @tailrec
    def countWalls(chars: List[Char], acc: Char = '.', count: Int = 0): Int = (chars, acc) match
      case (Nil, _) => count
      case ('|' :: chs, _) => countWalls(chs, '.', count + 1)
      case ('L' :: chs, _) => countWalls(chs, 'L', count)
      case ('F' :: chs, _) => countWalls(chs, 'F', count)
      case ('7' :: chs, 'L') => countWalls(chs, '.', count + 1)
      case ('7' :: chs, _) => countWalls(chs, '.', count)
      case ('J' :: chs, 'F') => countWalls(chs, '.', count + 1)
      case ('J' :: chs, _) => countWalls(chs, '.', count)
      case (ch :: chs, _) => countWalls(chs, acc, count)

    def isInside(x: Int, y: Int, loop: List[(Int, Int)]): Boolean = {
      !loop.contains(x, y) && (countWallsToTheRight(x, y) % 2 != 0)
    }

    (for {
      x <- input.indices
      y <- input.head.indices
      if isInside(x, y, loop)
    } yield 1).sum
  }
}

@main def day10(): Unit = {

  val input = readFile("resources/day10").map(_.toIndexedSeq).toIndexedSeq

  // Part 1

  val result1 = findLoopHalfLength(input)

  println(result1)

  // Part 2

  /**
   * Part 2 is way too slow!
   *
   * One day I might get back to it and optimize it :)
   *
   * For example, counting walls doesn't have to be quadratic -- we can just find beginning indexes of each
   * wall once per row, and use this knowledge to find the inside tiles.
   */

  val result2 = countInsideDots(input)

  println(result2)
}