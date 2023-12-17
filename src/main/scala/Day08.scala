import Util.readFile

import scala.annotation.tailrec

@main def day08(): Unit = {

  val input = readFile("resources/day08")

  val instructions = input.head

  val pattern = """(\w+)\s*=\s*\((\w+),\s*(\w+)\)""".r

  val map = input.drop(2).map {
    case pattern(group1, group2, group3) => group1 -> (group2, group3)
  }.toMap

  // Part 1

  @tailrec
  def doSteps(position: String = "AAA", count: Int = 0): Int = {
    if (position == "ZZZ") count
    else {
      val nextPosition = instructions.charAt(count % instructions.length) match
        case 'L' => map(position)._1
        case _ => map(position)._2

      doSteps(nextPosition, count + 1)
    }
  }

  val result1 = doSteps()

  println(result1)

  // Part 2

  @tailrec
  def findGhostPeriod(position: String,
                      count: Int = 0): Int = {

    position.last match
      case 'Z' => count
      case _ =>
        val nextPosition = instructions.charAt(count % instructions.length) match
          case 'L' => map(position)._1
          case _ => map(position)._2

        findGhostPeriod(nextPosition, count + 1)
  }

  @tailrec
  def findPrimeFactors(n: Int, divisor: Int = 2, factors: List[Int] = Nil): List[Int] = {
    if (n < 2) factors
    else if (n % divisor == 0) findPrimeFactors(n / divisor, divisor + 1, divisor :: factors)
    else findPrimeFactors(n, divisor + 1, factors)
  }

  val result2 = map.keys.filter(_.last == 'A').toList.flatMap { startPosition =>
    findPrimeFactors(findGhostPeriod(startPosition))
  }.distinct.map(BigInt.apply).product

  println(result2)
}