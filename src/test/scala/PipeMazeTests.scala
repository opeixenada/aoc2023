import PipeMaze.countLoopSteps
import org.scalatest.flatspec.AnyFlatSpec

class PipeMazeTests extends AnyFlatSpec {
  "countLoopSteps" should "pass scenario 1" in {

    val input =
      """
        |.....
        |.S-7.
        |.|.|.
        |.L-J.
        |.....
        |""".stripLeading().stripMargin.split("\n").map(_.toIndexedSeq).toIndexedSeq

    assert(countLoopSteps(input) == 4)
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

    assert(countLoopSteps(input) == 4)
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

    assert(countLoopSteps(input) == 8)
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

    assert(countLoopSteps(input) == 8)
  }

}
