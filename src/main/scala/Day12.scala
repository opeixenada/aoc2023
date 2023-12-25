import HotSprings.*
import Util.{readFile, withTimeLogging}

import scala.annotation.tailrec
import scala.collection.immutable.::
import scala.collection.mutable

object HotSprings {

  case class Frame(s: String, ns: List[Int], parent: Option[Frame] = None) {
    lazy val cacheKey: (String, List[Int]) = s -> ns
  }

  private val cache: mutable.Map[(String, List[Int]), BigInt] = mutable.Map.empty
  private val maybeMatchesCache: mutable.Map[Frame, Boolean] = mutable.Map.empty

  private def prepareString(s: String) = s.dropWhile(_ == '.').reverse.dropWhile(_ == '.').reverse

  def countArrangements(s: String, ns: List[Int]): BigInt = {
    cache.clear()
    val trimmedString = prepareString(s)
    countArrangements(List(Frame(trimmedString, ns)))
    cache(trimmedString -> ns)
  }

  def countFoldedArrangements(s: String, ns: List[Int]): BigInt = {
    val repeatedString = List.fill(5)(s).mkString("?")
    val repeatedList = List.fill(5)(ns).flatten
    countArrangements(repeatedString, repeatedList)
  }

  @tailrec
  private def lengthsMatch(as: List[Int], bs: List[Int]): Boolean = (as, bs) match
    case (Nil, _) => true
    case (_, Nil) => true
    case (x :: xs, y :: ys) if x >= y => lengthsMatch(xs, ys)
    case (x :: xs, y :: ys) if x < y => lengthsMatch(x :: xs, ys)

  private def maybeMatches(s: String, ns: List[Int]): Boolean = ns match
    case _ if s.count(_ != '.') < ns.sum => false
    case _ if s.length < (ns.sum + ns.length - 1) => false
    case _ if "(#)+".r.findAllMatchIn(s).map(_.matched.length).maxOption.getOrElse(0) > ns.max => false
    case _ if !lengthsMatch("[#?]+".r.findAllMatchIn(s).map(_.matched.length).toList, ns) => false
    case _ => true

  @tailrec
  private def updateCache(frame: Frame, result: BigInt): Unit = {
    cache += (frame.cacheKey -> (result + cache.getOrElse(frame.cacheKey, 0)))
    frame.parent match
      case Some(parentState) => updateCache(parentState, result)
      case _ => // do nothing
  }

  @tailrec
  private def countArrangements(stack: List[Frame]): Unit = {
    stack match
      case Nil =>
        // nothing in the stack, return
        ()

      case (f@Frame(string, Nil, _)) :: fs if string.forall(_ != '#') =>
        // happy terminal state! we found a valid arrangement
        updateCache(f, 1)
        countArrangements(fs)

      case (f@Frame(string, Nil, _)) :: fs =>
        // there are still symbols left in the string
        // but the numbers list is empty
        // we arrived to an invalid arrangement
        updateCache(f, 0)
        countArrangements(fs)

      case (f@Frame(string, ns, _)) :: ss if string.isEmpty =>
        // there are still numbers left
        // but the string is empty
        // we arrived to an invalid arrangement
        updateCache(f, 0)
        countArrangements(ss)

      case f :: fs if cache.contains(f.cacheKey) =>
        f.parent.foreach(parentState => updateCache(parentState, cache(f.cacheKey)))
        countArrangements(fs)

      case (f@Frame(string, ns, _)) :: fs if !maybeMatches(string, ns) =>
        // discard
        updateCache(f, 0)
        countArrangements(fs)

      case (f@Frame(string, ns, _)) :: fs => string.head match
        case '#' if string.take(ns.head).forall(_ != '.') && !string.drop(ns.head).headOption.contains('#') =>
          // beginning of the string matches the first # group from the list
          // chop it off and create a new frame
          val newFrame = Frame(string.drop(ns.head + 1).dropWhile(_ == '.'), ns.tail, Some(f))
          countArrangements(newFrame :: fs)

        case '#' =>
          // beginning of the string doesn't match the first # group
          // discard this stack frame
          updateCache(f, 0)
          countArrangements(fs)

        case '?' =>
          // split to 2 new frames
          val newFrames = List(
            Frame(string.tail.dropWhile(_ == '.'), ns, Some(f)),
            Frame('#' + string.tail, ns, Some(f))
          )
          countArrangements(newFrames ++ fs)

        case ch => throw RuntimeException(s"Something wrong, we shouldn't have `$ch` here")
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

  withTimeLogging {
    val result2 = input.map(countFoldedArrangements).sum

    println(result2)
  }
}