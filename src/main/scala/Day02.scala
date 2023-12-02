import Util.readFile

@main def day02(): Unit = {

  val input = readFile("resources/day02")

  // Part 1

  val target = Map(
    "red" -> 12,
    "green" -> 13,
    "blue" -> 14
  )

  val colorRegex = "(\\d+) (red|green|blue)".r
  val gameNumberRegex = "Game (\\d+): ".r

  def extractColorCounts(input: String): Map[String, Int] = {
    colorRegex
      .findAllMatchIn(input)
      .map { m => m.group(2) -> m.group(1).toInt }
      .toMap
      .withDefaultValue(0)
  }

  val games: Map[Int, List[Map[String, Int]]] = input.map { line =>
    val gameNumber = gameNumberRegex.findFirstMatchIn(line).map(_.group(1).toInt).get
    val colorCounts = line.substring(line.indexOf(':') + 1).split(";").toList.map(extractColorCounts)
    gameNumber -> colorCounts
  }.toMap

  def isGameGood(game: List[Map[String, Int]]): Boolean = {
    game.forall {
      _.forall { case (color, count) =>
        count <= target(color)
      }
    }
  }

  val result1 = games.foldLeft(0) { case (acc, (gameId, game)) =>
    if (isGameGood(game)) acc + gameId else acc
  }

  println(result1)

  // Part 2

  val result2 = games.values.foldLeft(0) { case (acc, game) =>
    val (minRed, minGreen, minBlue) = game.foldLeft((0, 0, 0)) { case ((r, g, b), round) =>
      (
        Math.max(r, round("red")),
        Math.max(g, round("green")),
        Math.max(b, round("blue"))
      )
    }

    acc + (minRed * minGreen * minBlue)
  }

  println(result2)
}