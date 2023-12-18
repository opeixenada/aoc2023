import Util.readFile

import scala.annotation.tailrec
import PipeMaze.*

import scala.collection.immutable.Set

object PipeMaze {

  private def invert(direction: Char): Char = direction match
    case '>' => '<'
    case '^' => 'v'
    case 'v' => '^'
    case _ => '>'

  private def getDirections(ch: Char): Seq[Char] = ch match
    case '|' => Seq('^', 'v')
    case '-' => Seq('<', '>')
    case 'L' => Seq('^', '>')
    case 'J' => Seq('^', '<')
    case '7' => Seq('<', 'v')
    case 'F' => Seq('v', '>')
    case _ => Nil

  private val allDirections: Seq[Char] = Seq('^', '>', 'v', '<')

  def countLoopSteps(input: IndexedSeq[IndexedSeq[Char]]): Int = {

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

    def findConnected(directions: Seq[Char], x: Int, y: Int) = directions.flatMap { direction =>
      findConnectedInDirection(direction, x, y).map { cs => direction -> cs }
    }

    val startingPoints: Seq[(Char, (Int, Int))] = findConnected(allDirections, sX, sY)

    @tailrec
    def traverse(state: Seq[(Char, (Int, Int))], count: Int = 0, traversed: List[(Int, Int)] = Nil): Int = {
      if (state.map(_._2).exists(traversed.contains(_))) count
      else {
        val nextState = state.flatMap { case (direction, (x, y)) =>
          findConnected(getDirections(input(x)(y)).filterNot(_ == invert(direction)), x, y).headOption
        }
        traverse(nextState, count + 1, traversed ++ state.map(_._2))
      }
    }

    traverse(startingPoints)
  }
}

@main def day10(): Unit = {

  val input = readFile("resources/day10").map(_.toIndexedSeq).toIndexedSeq

  // Part 1

  val result1 = countLoopSteps(input)

  println(result1)

  // Part 2

  val result2 = "foo"

  println(result2)
}