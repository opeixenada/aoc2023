import PipeMaze.{countInsideDots, findLoopHalfLength}
import org.scalatest.flatspec.AnyFlatSpec

class PipeMazeTests extends AnyFlatSpec {
  "findLoopHalfLength" should "pass scenario 1" in {

    val input =
      """
        |.....
        |.S-7.
        |.|.|.
        |.L-J.
        |.....
        |""".stripLeading().stripMargin.split("\n").map(_.toIndexedSeq).toIndexedSeq

    assert(findLoopHalfLength(input) == 4)
  }

  it should "pass scenario 2" in {

    val input =
      """
        |-L|F7
        |7S-7|
        |L|7||
        |-L-J|
        |L|-JF
        |""".stripLeading().stripMargin.split("\n").map(_.toIndexedSeq).toIndexedSeq

    assert(findLoopHalfLength(input) == 4)
  }

  it should "pass scenario 3" in {

    val input =
      """
        |..F7.
        |.FJ|.
        |SJ.L7
        ||F--J
        |LJ...
        |""".stripLeading().stripMargin.split("\n").map(_.toIndexedSeq).toIndexedSeq

    assert(findLoopHalfLength(input) == 8)
  }

  it should "pass scenario 4" in {

    val input =
      """
        |7-F7-
        |.FJ|7
        |SJLL7
        ||F--J
        |LJ.LJ
        |""".stripLeading().stripMargin.split("\n").map(_.toIndexedSeq).toIndexedSeq

    assert(findLoopHalfLength(input) == 8)
  }

  "countInsideDots" should "pass scenario 1" in {

    val input =
      """
        |...........
        |.S-------7.
        |.|F-----7|.
        |.||.....||.
        |.||.....||.
        |.|L-7.F-J|.
        |.|..|.|..|.
        |.L--J.L--J.
        |...........
        |""".stripLeading().stripMargin.split("\n").map(_.toIndexedSeq).toIndexedSeq

    assert(countInsideDots(input) == 4)
  }

  it should "pass scenario 2" in {

    val input =
      """
        |..........
        |.S------7.
        |.|F----7|.
        |.||OOOO||.
        |.||OOOO||.
        |.|L-7F-J|.
        |.|II||II|.
        |.L--JL--J.
        |..........
        |""".stripLeading().stripMargin.split("\n").map(_.toIndexedSeq).toIndexedSeq

    assert(countInsideDots(input) == 4)
  }

  it should "pass scenario 3" in {

    val input =
      """
        |.F----7F7F7F7F-7....
        |.|F--7||||||||FJ....
        |.||.FJ||||||||L7....
        |FJL7L7LJLJ||LJ.L-7..
        |L--J.L7...LJS7F-7L7.
        |....F-J..F7FJ|L7L7L7
        |....L7.F7||L7|.L7L7|
        |.....|FJLJ|FJ|F7|.LJ
        |....FJL-7.||.||||...
        |....L---J.LJ.LJLJ...
        |""".stripLeading().stripMargin.split("\n").map(_.toIndexedSeq).toIndexedSeq

    assert(countInsideDots(input) == 8)
  }

  it should "pass scenario 4" in {

    val input =
      """
        |FF7FSF7F7F7F7F7F---7
        |L|LJ||||||||||||F--J
        |FL-7LJLJ||||||LJL-77
        |F--JF--7||LJLJ7F7FJ-
        |L---JF-JLJ.||-FJLJJ7
        ||F|F-JF---7F7-L7L|7|
        ||FFJF7L7F-JF7|JL---7
        |7-L-JL7||F7|L7F-7F7|
        |L.L7LFJ|||||FJL7||LJ
        |L7JLJL-JLJLJL--JLJ.L
        |""".stripLeading().stripMargin.split("\n").map(_.toIndexedSeq).toIndexedSeq

    assert(countInsideDots(input) == 10)
  }
}
