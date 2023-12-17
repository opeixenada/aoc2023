import CamelCards.*
import org.scalatest.flatspec.AnyFlatSpec

class CamelCardsTests extends AnyFlatSpec {
  "handRankWithJoker" should "be 6 for Five of a Kind" in {
    Seq(
      "22222",
      "2222J",
      "222JJ",
      "22JJJ",
      "2JJJJ",
      "JJJJJ",
    ).foreach { hand =>
      assert(handRankWithJoker(hand) == 6)
    }
  }

  it should "be 5 for Four of a Kind" in {
    Seq(
      "22223",
      "222J3",
    ).foreach { hand =>
      assert(handRankWithJoker(hand) == 5)
    }
  }

  it should "be 4 for Full House" in {
    Seq(
      "22333",
      "2233J",
    ).foreach { hand =>
      assert(handRankWithJoker(hand) == 4)
    }
  }

  it should "be 3 for Three of a Kind" in {
    Seq(
      "TTT98",
      "TTJ98",
      "TJJ98",
    ).foreach { hand =>
      assert(handRankWithJoker(hand) == 3)
    }
  }

  it should "be 2 for Two Pair" in {
    Seq(
      "23432",
    ).foreach { hand =>
      assert(handRankWithJoker(hand) == 2)
    }
  }

  it should "be 1 for One Pair" in {
    Seq(
      "A23A4",
      "A23J4",
    ).foreach { hand =>
      assert(handRankWithJoker(hand) == 1)
    }
  }
}
