import Fertilizer.*
import org.scalatest.flatspec.AnyFlatSpec

import scala.math.BigInt

class FertilizerTests extends AnyFlatSpec {
  "convertInterval" should "not convert non-overlapping interval on the left" in {
    val a = 1
    val sourceLength = 3

    val c = 5
    val length = 3

    assert(
      convertInterval((10, c, length), (a, sourceLength)) === ConversionResult(
        converted = Nil,
        remainder = List(BigInt(1) -> 3)
      )
    )
  }

  it should "not convert non-overlapping interval on the right" in {
    val a = 5
    val sourceLength = 3

    val c = 1
    val length = 3

    assert(
      convertInterval((10, c, length), (a, sourceLength)) === ConversionResult(
        converted = Nil,
        remainder = List(BigInt(5) -> 3)
      )
    )
  }

  it should "convert fully overlapping interval" in {
    val a = 3
    val sourceLength = 3

    val c = 1
    val length = 7

    assert(
      convertInterval((10, c, length), (a, sourceLength)) === ConversionResult(
        converted = List(BigInt(12) -> 3),
        remainder = Nil
      )
    )
  }

  it should "convert equal interval" in {
    val a = 1
    val sourceLength = 7

    val c = 1
    val length = 7

    assert(
      convertInterval((10, c, length), (a, sourceLength)) === ConversionResult(
        converted = List(BigInt(10) -> 7),
        remainder = Nil
      )
    )
  }

  it should "convert interval overlapping on the left" in {
    val a = 1
    val sourceLength = 3

    val c = 2
    val length = 3

    assert(
      convertInterval((10, c, length), (a, sourceLength)) === ConversionResult(
        converted = List(BigInt(10) -> 2),
        remainder = List(BigInt(1) -> 1)
      )
    )
  }

  it should "convert enveloping interval" in {
    val a = 1
    val sourceLength = 6

    val c = 2
    val length = 3

    assert(
      convertInterval((10, c, length), (a, sourceLength)) === ConversionResult(
        converted = List(BigInt(10) -> 3),
        remainder = List(BigInt(1) -> 1, BigInt(5) -> 2)
      )
    )
  }

  it should "convert interval overlapping on the right" in {
    val a = 2
    val sourceLength = 3

    val c = 1
    val length = 3

    assert(
      convertInterval((10, c, length), (a, sourceLength)) === ConversionResult(
        converted = List(BigInt(11) -> 2),
        remainder = List(BigInt(4) -> 1)
      )
    )
  }

}
