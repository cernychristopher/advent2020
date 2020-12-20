import Input.ListOps

case class Tile(id: Int, lines: List[String]) {
  lazy val normalBorders: List[String] = List(
    lines.head,
    lines
      .map(_.last)
      .mkString,
    lines.last.reverse,
    lines.reverse.map(_.head).mkString
  )

  lazy val flippedBorders: List[String] = normalBorders.map(_.reverse)

  lazy val borders: Seq[String] = normalBorders ::: flippedBorders

  def adjacent(anotherTile: Tile): Boolean =
    borders.intersect(anotherTile.borders).nonEmpty
}

object Advent20 {

  def main(args: Array[String]): Unit = {
    val tiles = Input
      .byExercise(20)
      .separatedBy(_.isEmpty)
      .map(toTile)

    val neighborGrid = tiles.map { tile =>
      val adjacentTiles = tiles.filter { otherTile =>
        otherTile.id != tile.id && tile.adjacent(otherTile)
      }

      tile -> adjacentTiles
    }.toMap

    neighborGrid.foreach { case (key, neighbors) => println(s"${key.id}: ${neighbors.map(_.id)}") }

    val cornerGrids = neighborGrid.filter(_._2.length == 2).keys.map(_.id).toList

    val solution1 = cornerGrids.map(_.toLong).tapEach(println).product

    val solution2 = List(7)

    println(s"Solution1: $solution1")
    println(s"Solution2: ${solution2.sum}")
  }

  def toTile(in: List[String]): Tile = in match {
    case header :: lines => Tile(header.drop("Tile ".length).takeWhile(_.isDigit).toInt, lines)
  }
}
