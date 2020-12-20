import Input.ListOps

import scala.annotation.tailrec
import scala.util.matching.Regex

object Advent20 {

  type Image = List[String]

  def main(args: Array[String]): Unit = {
    val tiles = Input
      .byExercise(20)
      .separatedBy(_.isEmpty)
      .map(Tile.toTile)

    val neighborGrid: Map[Tile, List[Tile]] = tiles.map { tile =>
      val adjacentTiles = tiles.filter { otherTile =>
        otherTile.id != tile.id && tile.adjacentTo(otherTile)
      }

      tile -> adjacentTiles
    }.toMap

    // How to solve a puzzle? Start at the corners, that are those tiles that have only two neighbors!
    val cornerTiles = neighborGrid.filter(_._2.length == 2).keys.toList

    val solution1 = cornerTiles.map(_.id.toLong).product
    println(s"Solution1: $solution1")

    // How to continue solving a puzzle? Try to fit the outermost tiles together, so you have a contiguous border
    val borderTiles =
      neighborGrid.filter(_._2.length < 4).keys.toList
    val orderedBorder =
      computeBorder(cornerTiles.head, borderTiles, neighborGrid).reverse

    // In fact, we only have to build the first line of puzzle parts. Knowing which parts are adjencent to those
    // leads to the next line, and so on -> we built the whole grid out of that
    val firstLine = orderedBorder.take(orderedBorder.indexWhere(neighborGrid(_).size == 2, 1) + 1)
    val lines: List[List[Tile]] = allLines(firstLine, neighborGrid)

    def getTileAt(line: Int, column: Int): Option[Tile] = lines.lift(line).flatMap(_.lift(column))

    // We know the position of each tile on the puzzle grid, and thus also which Tiles are next to
    // each and every tile
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

    // we can now determine whether we have to flip/rotate every tile so that
    // it fits all its neighbors
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
          .map(
            Tile.stripBorder
          ) // after knowing the directions all images have to face, we can strip the border
          .get
      }
    }

    // finally we can combine all the tiles to one big image
    val imageOfTheSea = puzzleGrid
      .map { imageRow =>
        imageRow.reduce { (image, otherImage) =>
          image.zip(otherImage).map(pair => pair._1 + pair._2)
        }
      }
      .reduce(_ ::: _)

    // We do not know where the top of the image is, and also not if we still have to flip it!
    val finalImages = Tile.operations.map(_.apply(imageOfTheSea))

    // But we know: when the image is turned the right way, there should be some sea monster sightings
    // visible. So the final image must be the one, where there is at least one sea monster sighting!
    val (finalImage, sightings) =
      finalImages.map(image => image -> SeaMonster.sightings(image)).find(_._2.nonEmpty).get

    // It was easier for me to visualize the sea monster sightings in the original image - we could just have counted
    // the waves the sea monsters cover and be done as well.
    val withMarkedSightings = sightings.foldLeft(finalImage)(SeaMonster.replaceSeaMonster)

    // All waves ('#') not covered by sea monsters
    val solution2 = countWaves(withMarkedSightings)

    println(s"Solution2: $solution2")
  }

  def countWaves(image: Image): Int = image.map(_.count(_ == '#')).sum

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
        Option(line.map { t =>
          neighborGrid(t).find(!seenTiles.contains(_)).get
        })
      }

    @tailrec
    def go(acc: List[List[T]], line: List[T], seenTiles: List[T]): List[List[T]] =
      nextLine(line, seenTiles) match {
        case None           => acc
        case Some(nextLine) => go(acc :+ nextLine, nextLine, nextLine ::: seenTiles)
      }

    go(List(firstLine), firstLine, firstLine)
  }

  case class Neighbors(
    self: Tile,
    north: Option[Tile],
    south: Option[Tile],
    west: Option[Tile],
    east: Option[Tile]
  )

  object Tile {
    def toTile(in: List[String]): Tile = in match {
      case header :: lines => Tile(header.drop("Tile ".length).takeWhile(_.isDigit).toInt, lines)
    }

    def horizontalFlip(image: Image): Image = image.reverse
    def turnClockwise(image: Image): Image = image.head.indices.map { index =>
      image.reverse.map { line => line.charAt(index) }.mkString
    }.toList

    private val turns =
      LazyList.continually(turnClockwise _).scanLeft(identity[Image] _)(_ andThen _).take(4).toList

    val operations: Seq[Image => Image] = for {
      flip <- List(identity[Image] _, horizontalFlip _)
      rotate <- turns
    } yield rotate.andThen(flip)

    def northBorder(image: Image): String = image.head
    def southBorder(image: Image): String = image.last
    def eastBorder(image: Image): String = image.map(_.head).mkString
    def westBorder(image: Image): String = image.map(_.last).mkString

    def stripBorder(image: Image): Image =
      image.drop(1).dropRight(1).map(_.drop(1).dropRight(1))
  }

  case class Tile(id: Int, image: Image) {

    lazy val borders: Seq[String] = possibleImages.flatMap(_.headOption)
    lazy val possibleImages: Seq[Image] = Tile.operations.map(_.apply(image))

    def adjacentTo(anotherTile: Tile): Boolean =
      borders.intersect(anotherTile.borders).nonEmpty
  }

  object SeaMonster {
    type Sighting = (Int, Int) // row, column

    val seaMonsterImage: List[String] =
      """|                  #
         |#    ##    ##    ###
         | #  #  #  #  #  #   """.stripMargin.linesIterator.toList

    val seaMonsterRegexes: Seq[Regex] = seaMonsterImage
      .map(_.replace(' ', '.'))
      .map(_.r)

    def matchesSeaMonster(subImage: Image): List[Int] = {
      val regexToLine = seaMonsterRegexes.zip(subImage)

      subImage.head.indices.filter { start =>
        regexToLine.forall { case (regex, string) =>
          regex.findPrefixOf(string.drop(start)).isDefined
        }
      }.toList
    }

    def sightings(image: Image): List[Sighting] = image
      .sliding(3, 1)
      .zipWithIndex
      .flatMap { case (subImage, row) =>
        SeaMonster.matchesSeaMonster(subImage).map(column => row -> column)
      }
      .toList

    def replaceSeaMonster(image: Image, sighting: Sighting): Image = {
      val (line, column) = sighting
      val before = image.take(line)

      val replaced = image.slice(line, line + seaMonsterImage.length).zip(seaMonsterImage).map {
        case (imageLine, seaMonsterLine) =>
          imageLine.take(column) + imageLine
            .drop(column)
            .zip(seaMonsterLine)
            .map { case (imageChar, monsterChar) => if (monsterChar == '#') '0' else imageChar }
            .mkString + imageLine
            .drop(column + seaMonsterLine.length)
      }

      val after = image.drop(line + seaMonsterImage.length)

      before ::: replaced ::: after
    }
  }
}
