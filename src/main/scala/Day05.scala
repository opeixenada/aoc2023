import Util.readFile

import scala.annotation.tailrec
import Fertilizer._

object Fertilizer {

  case class ConversionResult(converted: List[(BigInt, BigInt)], remainder: List[(BigInt, BigInt)])

  def merge(x: ConversionResult, y: ConversionResult): ConversionResult =
    ConversionResult(x.converted ++ y.converted, x.remainder ++ y.remainder)

  def convertInterval(mapping: (BigInt, BigInt, BigInt), interval: (BigInt, BigInt)): ConversionResult = {
    val (destinationStart, c, length) = mapping
    val (a, sourceLength) = interval

    val b = a + sourceLength - 1
    val d = c + length - 1

    if (b < c || d < a) {
      // No overlap
      ConversionResult(Nil, List(interval))
    } else {
      val mappedStart = if (a < c) c else a
      val mappedEnd = if (b > d) d else b

      val unmappedLeft = if (a < c) Some(a -> (c - a)) else None
      val unmappedRight = if (b > d) Some((d + 1) -> (b - d)) else None

      val mappedLength = mappedEnd - mappedStart + 1

      ConversionResult(
        List(destinationStart + (mappedStart - c) -> mappedLength),
        List(unmappedLeft, unmappedRight).flatten
      )
    }
  }
}

@main def day05(): Unit = {

  val input = readFile("resources/day05")

  // Part 1

  val seeds = input.head.substring("seeds: ".length).split(' ').map(BigInt.apply).toList

  val maps: List[List[(BigInt, BigInt, BigInt)]] = input
    .drop(2)
    .foldLeft(List(List.empty[String])) { case (acc, s) =>
      if (s.endsWith("map:")) acc
      else if (s.isBlank) Nil :: acc
      else (s :: acc.head) :: acc.tail
    }
    .map {
      _.map { s =>
        val xs = s.split(' ').map(BigInt.apply)
        (xs(0), xs(1), xs(2))
      }
    }

  def mapValue(map: List[(BigInt, BigInt, BigInt)], x: BigInt): BigInt = {
    map
      .flatMap { case (destinationStart, sourceStart, length) =>
        val diff = x - sourceStart
        if (diff < 0 || diff >= length) None
        else Some(destinationStart + diff)
      }
      .headOption
      .getOrElse(x)
  }

  def findLocation(seed: BigInt): BigInt = {
    maps.foldRight(seed)(mapValue)
  }

  val result1 = seeds.map(findLocation).min

  println(result1)

  // Part 2

  @tailrec
  def getSeedIntervals(seeds: List[BigInt], intervals: List[(BigInt, BigInt)] = Nil): List[(BigInt, BigInt)] = {
    seeds match
      case x :: y :: xs => getSeedIntervals(xs, (x -> y) :: intervals)
      case _            => intervals
  }

  val seedIntervals = getSeedIntervals(seeds)

  def mapInterval(map: List[(BigInt, BigInt, BigInt)], intervals: List[(BigInt, BigInt)]): List[(BigInt, BigInt)] = {

    val conversionResult = map.foldLeft(ConversionResult(Nil, intervals)) { case (acc, mapping) =>
      val result = acc.remainder.map { interval =>
        convertInterval(mapping, interval)
      }.foldLeft(ConversionResult(Nil, Nil))(merge)

      ConversionResult(acc.converted ++ result.converted, result.remainder)
    }

    conversionResult.remainder ++ conversionResult.converted
  }


  def findIntervalLocation(interval: (BigInt, BigInt)): List[BigInt] = {
    maps.foldRight(List(interval))(mapInterval).map(_._1)
  }

  val result2 = seedIntervals.flatMap(findIntervalLocation).min

  println(result2)
}