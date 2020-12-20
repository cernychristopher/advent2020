import Input.ListOps

import scala.annotation.tailrec

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

    //neighborGrid.foreach { case (key, neighbors) => println(s"${key.id}: ${neighbors.map(_.id)}") }

    val cornerGrids = neighborGrid.filter(_._2.length == 2).keys.toList

    val solution1 = cornerGrids.map(_.id.toLong).product

    val borderTiles =
      neighborGrid.filter(_._2.length < 4).keys.toList

    computeBorder(cornerGrids.head, borderTiles, neighborGrid).foreach(println)

    val solution2 = List(7)

    println(s"Solution1: $solution1")
    println(s"Solution2: ${solution2.sum}")
  }

  def toTile(in: List[String]): Tile = in match {
    case header :: lines => Tile(header.drop("Tile ".length).takeWhile(_.isDigit).toInt, lines)
  }

  def computeBorder[T](corner: T, borderTiles: List[T], neighborGrid: Map[T, List[T]]): List[T] = {
    val inBorder = borderTiles.toSet

    println(borderTiles.size)

    @tailrec
    def go(acc: List[T], remaining: List[T]): List[T] = {
      val lastPoint = acc.head

      remaining match {
        case Nil => acc
        case _ =>
          val foo = neighborGrid(lastPoint).filter { otherPoint =>
            inBorder.contains(otherPoint) &&
            !acc.contains(otherPoint) &&
            remaining.contains(otherPoint)
          }

          if (foo.isEmpty) {
            println("FSCK")
          }

          val nextPoint = foo.head

          go(nextPoint :: acc, remaining.filterNot(_ == nextPoint))
      }
    }

    go(List(corner), borderTiles.filterNot(_ == corner))
  }
}
