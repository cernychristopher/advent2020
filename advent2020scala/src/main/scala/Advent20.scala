import Input.ListOps
import Tile.{Image, operations}

import scala.annotation.tailrec
import scala.util.matching.Regex

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

object SeaMonster {
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

    /*
    seaMonsterRegexes.head
      .findAllMatchIn(subImage.head)
      .map(_.start)
      .flatMap { start =>
        if (
          seaMonsterRegexes.zip(subImage).forall { case (regex, string) =>
            regex.findPrefixOf(string.drop(start)).isDefined
          }
        )
          Option(start)
        else None
      }
      .toList
     */
    /*

    val allSeaMonsterStarts =
      seaMonsterRegexes(2).findAllMatchIn(subImage._3).map(_.start).flatMap { start =>
        if (
          seaMonsterRegexes(1).findPrefixOf(subImage._2.drop(start)).isDefined &&
          seaMonsterRegexes.head.findPrefixOf(subImage._1.drop(start)).isDefined
        ) Option(start)
        else None
      }

    allSeaMonsterStarts.toList

     */
  }

  def seaMonsterMatches(image: Image): List[(Int, Int)] = image
    .sliding(3, 1)
    .zipWithIndex
    .flatMap { case (subImage, row) =>
      SeaMonster.matchesSeaMonster(subImage).map(column => row -> column)
    }
    .toList

  def replaceSeaMonster(image: Image, sighting: (Int, Int)): Image = {
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

    val cornerTiles = neighborGrid.filter(_._2.length == 2).keys.toList

    val solution1 = cornerTiles.map(_.id.toLong).product
    println(s"Solution1: $solution1")

    val borderTiles =
      neighborGrid.filter(_._2.length < 4).keys.toList

    val orderedBorder =
      computeBorder(cornerTiles.head, borderTiles, neighborGrid).reverse
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
        val potentiallyCorrectImages = Tile.operations
          .map(_.apply(neighbor.self.image))
          .filter { image =>
            neighbor.north.forall(_.borders.contains(Tile.northBorder(image))) &&
            neighbor.south.forall(_.borders.contains(Tile.southBorder(image))) &&
            neighbor.east.forall(_.borders.contains(Tile.westBorder(image))) &&
            neighbor.west.forall(_.borders.contains(Tile.eastBorder(image)))
          }

        if (potentiallyCorrectImages.size > 1) {
          throw new Exception("We are not building this right")
        }

        Tile.stripBorder(potentiallyCorrectImages.head)
      }
    }

    val finalImage = puzzleGrid
      .map { imageRow =>
        imageRow.reduce { (image, otherImage) =>
          image.zip(otherImage).map(pair => pair._1 + pair._2)
        }
      }
      .reduce(_ ::: _)

    val finalImages = operations.map(_.apply(finalImage))
    val (rotatedFinalImage, sightings) =
      finalImages.map(image => image -> SeaMonster.seaMonsterMatches(image)).find(_._2.nonEmpty).get

    println(s"${sightings.length} sightings")

    val withMarkedSightings = sightings.foldLeft(rotatedFinalImage)(SeaMonster.replaceSeaMonster)

    rotatedFinalImage.foreach(println)
    sightings.foreach(println)
    withMarkedSightings.foreach(println)

    val originalWaves = countWaves(rotatedFinalImage)
    val coveredBySighting = countWaves(SeaMonster.seaMonsterImage)
    val solution2 = countWaves(withMarkedSightings)

    println(s"Expected: ${originalWaves - coveredBySighting * sightings.size}")

    println(s"Solution2: $solution2")
  }

  def countWaves(image: Image): Int = image.map(_.count(_ == '#')).sum

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
        Option(line.map { t =>
          val candidates = neighborGrid(t).filter(!seenTiles.contains(_))
          if (candidates.size > 1) throw new Exception("Unable to compute next line!")
          candidates.head
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
}
