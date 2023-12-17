import CamelCards.{HandsOrdering, HandsOrderingWithJoker}
import Util.readFile

object CamelCards {

  private def cardRank(x: Char): Int = x match
    case 'A' => 14
    case 'K' => 13
    case 'Q' => 12
    case 'J' => 11
    case 'T' => 10
    case _ if x.isDigit => x - '0'
    case _ => 0

  private def isFiveOfAKind(hand: Seq[String]): Boolean = hand.size == 1

  private def isFourOfAKind(hand: Seq[String]): Boolean = hand.exists(_.length == 4)

  private def isFullHouse(hand: Seq[String]): Boolean = (hand.size == 2) && hand.exists(_.length == 3)

  private def isThreeOfAKind(hand: Seq[String]): Boolean = (hand.size == 3) && hand.exists(_.length == 3)

  private def isTwoPair(hand: Seq[String]): Boolean = hand.count(_.length == 2) == 2

  private def isOnePair(hand: Seq[String]): Boolean = (hand.size == 4) && hand.exists(_.length == 2)

  private def handRank(hand: String): Int = {
    hand.groupBy(identity).values.toSeq match
      case x if isFiveOfAKind(x) => 6
      case x if isFourOfAKind(x) => 5
      case x if isFullHouse(x) => 4
      case x if isThreeOfAKind(x) => 3
      case x if isTwoPair(x) => 2
      case x if isOnePair(x) => 1
      case _ => 0
  }

  implicit object HandsOrdering extends Ordering[String] {
    def compare(x: String, y: String): Int = {
      val result = handRank(x).compare(handRank(y))
      if (result != 0) result
      else {
        x.zip(y).map { case (a, b) =>
          cardRank(a).compare(cardRank(b))
        }.find(_ != 0).getOrElse(0)
      }
    }
  }

  private def cardRankWithJoker(x: Char): Int = x match
    case 'A' => 14
    case 'K' => 13
    case 'Q' => 12
    case 'T' => 10
    case _ if x.isDigit => x - '0'
    case _ => 0

  private def canBeFiveOfAKind(hand: Seq[String], jokers: Int): Boolean =
    hand.exists(_.length == (5 - jokers)) || (jokers == 5)

  private def canBeFourOfAKind(hand: Seq[String], jokers: Int): Boolean = hand.exists(_.length == (4 - jokers))

  private def canBeFullHouse(hand: Seq[String], jokers: Int): Boolean =
    isFullHouse(hand) || ((jokers == 1) && (hand.count(_.length == 2) == 2))

  private def canBeThreeOfAKind(hand: Seq[String], jokers: Int): Boolean = hand.exists(_.length == (3 - jokers))

  private def canBeTwoPair(hand: Seq[String], jokers: Int): Boolean = isTwoPair(hand)

  private def canBeOnePair(hand: Seq[String], jokers: Int): Boolean = isOnePair(hand) || (jokers == 1)

  def handRankWithJoker(hand: String): Int = {
    val groups = hand.groupBy(identity)
    val jokersCount = groups.get('J').map(_.length).getOrElse(0)

    (groups.filterNot(_._1 == 'J').values.toSeq, jokersCount) match
      case (x, js) if canBeFiveOfAKind(x, js) => 6
      case (x, js) if canBeFourOfAKind(x, js) => 5
      case (x, js) if canBeFullHouse(x, js) => 4
      case (x, js) if canBeThreeOfAKind(x, js) => 3
      case (x, js) if canBeTwoPair(x, js) => 2
      case (x, js) if canBeOnePair(x, js) => 1
      case _ => 0
  }

  implicit object HandsOrderingWithJoker extends Ordering[String] {
    def compare(x: String, y: String): Int = {
      val result = handRankWithJoker(x).compare(handRankWithJoker(y))
      if (result != 0) result
      else {
        x.zip(y).map { case (a, b) =>
          cardRankWithJoker(a).compare(cardRankWithJoker(b))
        }.find(_ != 0).getOrElse(0)
      }
    }
  }

}

@main def day07(): Unit = {

  val input = readFile("resources/day07").map { s =>
    val xs = s.split(' ')
    xs.head -> xs.tail.head.toInt
  }

  // Part 1

  val result1 = input.sortBy(_._1)(ord = HandsOrdering).zipWithIndex.map {
    case ((_, bid), rank) => bid * (rank + 1)
  }.sum

  println(result1)

  // Part 2

  val result2 = input.sortBy(_._1)(ord = HandsOrderingWithJoker).zipWithIndex.map { case ((_, bid), rank) =>
    bid * (rank + 1)
  }.sum

  println(result2)
}