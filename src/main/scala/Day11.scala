import Util.readFile

@main def day11(): Unit = {

  val input = readFile("resources/day11")

  // Part 1

  val galaxies: List[(BigInt, BigInt)] = input.zipWithIndex.flatMap { case (row, x) =>
    row.zipWithIndex.filter { case (ch, y) => ch != '.' }.map { case (_, y) => BigInt(x) -> BigInt(y) }
  }

  val emptyRows = input.indices.filterNot(galaxies.map(_._1).contains)
  val emptyColumns = input.head.indices.filterNot(galaxies.map(_._2).contains)

  def expand(gs: List[(BigInt, BigInt)], times: BigInt): List[(BigInt, BigInt)] = gs.map { case (x, y) =>
    val emptyRowsBefore = emptyRows.count(_ < x)
    val emptyColumnsBefore = emptyColumns.count(_ < y)

    (x + emptyRowsBefore * (times - 1)) -> (y + emptyColumnsBefore * (times - 1))
  }

  def getPairs[A](xs: List[A]): List[(A, A)] = xs.tails.toList.tail.foldLeft(List.empty[(A, A)]) { case (acc, ys) =>
    acc ++ xs.zip(ys)
  }

  def findDistance(a: (BigInt, BigInt), b: (BigInt, BigInt)): BigInt = (a._1 - b._1).abs + (a._2 - b._2).abs

  val result1 = getPairs(expand(galaxies, 2)).map(findDistance).sum

  println(result1)

  // Part 2

  val result2 = getPairs(expand(galaxies, 1000000)).map(findDistance).sum

  println(result2)
}