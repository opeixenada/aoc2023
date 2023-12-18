import MirageMaintenance.{extrapolateBackwards, extrapolateForwards}
import org.scalatest.flatspec.AnyFlatSpec

class MirageMaintenanceTests extends AnyFlatSpec {
  "extrapolateForwards" should "pass test 1" in {
    val xs = "0 3 6 9 12 15".split(' ').map(_.toInt).toList
    assert(extrapolateForwards(xs) == 18)
  }

  it should "pass test 2" in {
    val xs = "1 3 6 10 15 21".split(' ').map(_.toInt).toList
    assert(extrapolateForwards(xs) == 28)
  }

  it should "pass test 3" in {
    val xs = "10 13 16 21 30 45".split(' ').map(_.toInt).toList
    assert(extrapolateForwards(xs) == 68)
  }

  "extrapolateBackwards" should "pass test 1" in {
    val xs = "10 13 16 21 30 45".split(' ').map(_.toInt).toList
    assert(extrapolateBackwards(xs) == 5)
  }
}
