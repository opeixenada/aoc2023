import Util.splitBy
import org.scalatest.flatspec.AnyFlatSpec

class PointOfIncidenceTests extends AnyFlatSpec {
  "findSymmetryAxis" should "pass test 1" in {

    val input =
      """|#.##..##.
         |..#.##.#.
         |##......#
         |##......#
         |..#.##.#.
         |..##..##.
         |#.#.##.#.
         |""".stripMargin.split("\n").toList

    assert(PointOfIncidence.getAxisScore(input).contains(5))
  }

  it should "pass reversed test 1" in {

    val input =
      """|#.##..##.
         |..#.##.#.
         |##......#
         |##......#
         |..#.##.#.
         |..##..##.
         |#.#.##.#.
         |""".stripMargin.split("\n").toList.map(_.reverse)

    assert(PointOfIncidence.getAxisScore(input).contains(4))
  }

  it should "pass test 2" in {

    val input =
      """|#...##..#
         |#....#..#
         |..##..###
         |#####.##.
         |#####.##.
         |..##..###
         |#....#..#
         |""".stripMargin.split("\n").toList

    assert(PointOfIncidence.getAxisScore(input).contains(400))
  }

  it should "pass reversed test 2" in {

    val input =
      """|#...##..#
         |#....#..#
         |..##..###
         |#####.##.
         |#####.##.
         |..##..###
         |#....#..#
         |""".stripMargin.split("\n").toList.reverse

    assert(PointOfIncidence.getAxisScore(input).contains(300))
  }

  it should "pass test 3" in {

    val input =
      """|..######.
         |..######.
         |....#..##
         |##.....##
         |##..#...#
         |..##.....
         |.#..#.#..
         |.#..###..
         |..##.....
         |""".stripMargin.split("\n").toList

    assert(PointOfIncidence.getAxisScore(input).contains(100))
  }

  it should "pass test 4" in {

    val input =
      """|#..#.#........#
         |#..######..####
         |.##..#.#.##.#.#
         |#..##..........
         |######........#
         |#..####......##
         |.##.##.#...##.#
         |""".stripMargin.split("\n").toList

    assert(PointOfIncidence.getAxisScore(input).contains(2))
  }

  it should "pass test 5" in {

    val input =
      """|...#..#........
         |.#.#..#.#.##.#.
         |..#....#..##..#
         |.#..##..#....#.
         |..######......#
         |...#..#..#..#..
         |...#..#........
         |#...##...#..#..
         |.##.##.##....##
         |...#..#...##...
         |#..####..####..
         |..#.##.#......#
         |..#.##.#......#
         |###....###..###
         |#........####..
         |""".stripMargin.split("\n").toList

    assert(PointOfIncidence.getAxisScore(input).contains(11))
  }

  it should "pass test 6" in {

    val input =
      """|#..##..
         |..#..#.
         |.######
         |..####.
         |..####.
         |##....#
         |.#.##.#
         |.#.##.#
         |##....#
         |..####.
         |..####.
         |.######
         |#.#..#.
         |#..##..
         |#.#..#.
         |""".stripMargin.split("\n").toList

    assert(PointOfIncidence.getAxisScore(input).contains(4))
  }

  "findAlternativeSymmetryAxis" should "pass test 1" in {

    val input =
      """|#.##..##.
         |..#.##.#.
         |##......#
         |##......#
         |..#.##.#.
         |..##..##.
         |#.#.##.#.
         |""".stripMargin.split("\n").toList

    assert(PointOfIncidence.getAlternativeAxisScore(input).contains(300))
  }

  it should "pass test 2" in {

    val input =
      """|#...##..#
         |#....#..#
         |..##..###
         |#####.##.
         |#####.##.
         |..##..###
         |#....#..#
         |""".stripMargin.split("\n").toList

    assert(PointOfIncidence.getAlternativeAxisScore(input).contains(100))
  }

  it should "pass test 3" in {

    val input =
      """|#..#.#........#
         |#..######..####
         |.##..#.#.##.#.#
         |#..##..........
         |######........#
         |#..####......##
         |.##.##.#...##.#
         |""".stripMargin.split("\n").toList

    assert(PointOfIncidence.getAlternativeAxisScore(input).isDefined)
  }

  it should "pass test 4" in {

    val input =
      """|#..##..
         |..#..#.
         |.######
         |..####.
         |..####.
         |##....#
         |.#.##.#
         |.#.##.#
         |##....#
         |..####.
         |..####.
         |.######
         |#.#..#.
         |#..##..
         |#.#..#.
         |""".stripMargin.split("\n").toList

    assert(PointOfIncidence.getAlternativeAxisScore(input).contains(700))
  }

  "scores" should "be calculated correctly" in {
    val fileContent: List[String] =
      """|#.##..##.
         |..#.##.#.
         |##......#
         |##......#
         |..#.##.#.
         |..##..##.
         |#.#.##.#.
         |
         |#...##..#
         |#....#..#
         |..##..###
         |#####.##.
         |#####.##.
         |..##..###
         |#....#..#
         |
         |.#.##.#.#
         |.##..##..
         |.#.##.#..
         |#......##
         |#......##
         |.#.##.#..
         |.##..##.#
         |
         |#..#....#
         |###..##..
         |.##.#####
         |.##.#####
         |###..##..
         |#..#....#
         |#..##...#
         |""".stripMargin.split("\n").toList

    val input = splitBy(fileContent, { (x: String) => x.isBlank })

    assert(input.flatMap(PointOfIncidence.getAxisScore).sum == 709)
    assert(input.flatMap(PointOfIncidence.getAlternativeAxisScore).sum == 1400)
  }
}
