import Input.ListOps
import Tile.Image

import scala.annotation.tailrec

object Tile {
  type Image = List[String]

  def flipHorizontal(image: Image): Image = image.reverse
  def turnClockwise(image: Image): Image = image.head.indices.map { index =>
    image.reverse.map { line => line.charAt(index) }.mkString
  }.toList

  def turnCounterClockwise(image: Image): Image = turnClockwise(turnClockwise(turnClockwise(image)))

  val operations: Seq[Image => Image] = for {
    flip <- List(identity[Image] _, flipHorizontal _)
    rotate <- List(
      identity[Image] _,
      turnClockwise _,
      (turnClockwise _).andThen(turnClockwise),
      (turnClockwise _).andThen(turnClockwise).andThen(turnClockwise)
    )
  } yield rotate.andThen(flip)

  def northBorder(image: Image): String = image.head
  def southBorder(image: Image): String = image.last
  def eastBorder(image: Image): String = image.map(_.head).mkString
  def westBorder(image: Image): String = image.map(_.last).mkString

  def stripBorder(image: Image): Image =
    image.drop(1).dropRight(1).map(_.drop(1).dropRight(1))
}

case class Tile(id: Int, image: Image) {
  lazy val normalBorders: List[String] = List(
    image.head,
    image
      .map(_.last)
      .mkString,
    image.last.reverse,
    image.reverse.map(_.head).mkString
  )
  lazy val flippedBorders: List[String] = normalBorders.map(_.reverse)
  lazy val borders: Seq[String] = normalBorders ::: flippedBorders

  lazy val possibleImages: Seq[Image] = Tile.operations.map(_.apply(image))

  def adjacent(anotherTile: Tile): Boolean =
    borders.intersect(anotherTile.borders).nonEmpty
}

case class Neighbors(
  self: Tile,
  north: Option[Tile],
  south: Option[Tile],
  west: Option[Tile],
  east: Option[Tile]
)

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

    val cornerGrids = neighborGrid.filter(_._2.length == 2).keys.toList

    val solution1 = cornerGrids.map(_.id.toLong).product

    val borderTiles =
      neighborGrid.filter(_._2.length < 4).keys.toList

    val orderedBorder =
      computeBorder(cornerGrids.find(_.id == 1951).get, borderTiles, neighborGrid).reverse
    val firstLine = orderedBorder.take(orderedBorder.indexWhere(neighborGrid(_).size == 2, 1) + 1)
    val lines = allLines(firstLine, neighborGrid)
    def getTileAt(line: Int, column: Int): Option[Tile] = lines.lift(line).flatMap(_.lift(column))

    val withNeighbors = lines.indices.map { lineIndex =>
      lines(lineIndex).indices.map { columnIndex =>
        Neighbors(
          self = getTileAt(lineIndex, columnIndex).get,
          north = getTileAt(lineIndex - 1, columnIndex),
          south = getTileAt(lineIndex + 1, columnIndex),
          west = getTileAt(lineIndex, columnIndex - 1),
          east = getTileAt(lineIndex, columnIndex + 1)
        )
      }
    }

    val puzzleGrid = withNeighbors.map { line =>
      line.map { neighbor =>
        Tile.operations
          .map(_.apply(neighbor.self.image))
          .find { image =>
            neighbor.north.forall(_.borders.contains(Tile.northBorder(image))) &&
            neighbor.south.forall(_.borders.contains(Tile.southBorder(image))) &&
            neighbor.east.forall(_.borders.contains(Tile.westBorder(image))) &&
            neighbor.west.forall(_.borders.contains(Tile.eastBorder(image)))
          }
          .get
      }
    }

    val solution2 = List(7)

    println(s"Solution1: $solution1")
    println(s"Solution2: ${solution2.sum}")
  }

  def toTile(in: List[String]): Tile = in match {
    case header :: lines => Tile(header.drop("Tile ".length).takeWhile(_.isDigit).toInt, lines)
  }

  def computeBorder[T](corner: T, borderTiles: List[T], neighborGrid: Map[T, List[T]]): List[T] = {
    val inBorder = borderTiles.toSet

    @tailrec
    def go(acc: List[T], remaining: List[T]): List[T] = {
      val lastPoint = acc.head

      remaining match {
        case Nil => acc
        case _ =>
          val nextPoint = neighborGrid(lastPoint).filter { otherPoint =>
            inBorder.contains(otherPoint) &&
            !acc.contains(otherPoint) &&
            remaining.contains(otherPoint)
          }.head

          go(nextPoint :: acc, remaining.filterNot(_ == nextPoint))
      }
    }

    go(List(corner), borderTiles.filterNot(_ == corner))
  }

  def allLines[T](firstLine: List[T], neighborGrid: Map[T, List[T]]): List[List[T]] = {
    def nextLine(line: List[T], seenTiles: List[T]): Option[List[T]] =
      if (seenTiles.size == neighborGrid.size) None
      else {
        Option(line.map { t => neighborGrid(t).find(!seenTiles.contains(_)).get })
      }

    @tailrec
    def go(acc: List[List[T]], line: List[T], seenTiles: List[T]): List[List[T]] =
      nextLine(line, seenTiles) match {
        case None           => acc
        case Some(nextLine) => go(acc :+ nextLine, nextLine, nextLine ::: seenTiles)
      }

    go(List(firstLine), firstLine, firstLine)
  }
}
