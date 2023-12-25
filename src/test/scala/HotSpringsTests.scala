import HotSprings.{countArrangements, countFoldedArrangements}
import org.scalatest.flatspec.AnyFlatSpec

class HotSpringsTests extends AnyFlatSpec {
  "countArrangements" should "pass scenario 1" in {
    assert(countArrangements("???.###", List(1, 1, 3)) == 1)
  }

  it should "pass scenario 2" in {
    assert(countArrangements(".??..??...?##.", List(1, 1, 3)) == 4)
  }

  it should "pass scenario 3" in {
    assert(countArrangements("?#?#?#?#?#?#?#?", List(1, 3, 1, 6)) == 1)
  }

  it should "pass scenario 4" in {
    assert(countArrangements("????.#...#...", List(4, 1, 1)) == 1)
  }

  it should "pass scenario 5" in {
    assert(countArrangements("????.######..#####.", List(1, 6, 5)) == 4)
  }

  it should "pass scenario 6" in {
    assert(countArrangements("?###????????", List(3, 2, 1)) == 10)
  }

  "countFoldedArrangements" should "pass scenario 1" in {
    assert(countFoldedArrangements("???.###", List(1, 1, 3)) == 1)
  }

  it should "pass scenario 2" in {
    assert(countFoldedArrangements(".??..??...?##.", List(1, 1, 3)) == 16384)
  }

  it should "pass scenario 3" in {
    assert(countFoldedArrangements("?#?#?#?#?#?#?#?", List(1, 3, 1, 6)) == 1)
  }

  it should "pass scenario 4" in {
    assert(countFoldedArrangements("????.#...#...", List(4, 1, 1)) == 16)
  }

  it should "pass scenario 5" in {
    assert(countFoldedArrangements("????.######..#####.", List(1, 6, 5)) == 2500)
  }

  it should "pass scenario 6" in {
    assert(countFoldedArrangements("?###????????", List(3, 2, 1)) == 506250)
  }
}
