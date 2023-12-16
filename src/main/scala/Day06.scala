import Util.readFile

@main def day06(): Unit = {

  val input = readFile("resources/day06")

  // Part 1

  def parse(s: String): Array[Double] = s.dropWhile(!_.isDigit).split("\\s+").map(_.toDouble)

  val races = parse(input.head).zip(parse(input.tail.head))

  /** `((t - x) * x) > d`
      *
      * `x^2 - tx + d < 0`
      */
  def countWinningStrategies(time: Double, distance: Double): Double = {

    val discriminant = time * time - 4 * distance

    if (discriminant < 0) {
      // No real roots, inequality is false for x for a > 0 (and in our case, a = 1)
      0
    } else {
      val root1 = (time + Math.sqrt(discriminant)) / 2
      val root2 = (time - Math.sqrt(discriminant)) / 2

      val min = (Seq(root1, root2, time - 1).min + 1).floor
      val max = (Seq(root1, root2, 1).max - 1).ceil

      max - min + 1
    }
  }

  val result1 = BigDecimal(races.map(countWinningStrategies).product)

  println(result1)

  // Part 2

  def parse2(s: String): Double = s.filter(_.isDigit).toDouble

  val result2 = BigDecimal(countWinningStrategies(parse2(input.head), parse2(input.tail.head)))

  println(result2)
}