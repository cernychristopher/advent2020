object Advent11 {
  type SeatLayout = Array[Array[Char]]

  def main(args: Array[String]): Unit = {
    val input = Input.byExercise(11).map(_.toCharArray).toArray

    val finalLayout1 = finalLayout(input, nextLayout)
    val finalLayout2 = finalLayout(input, nextLayout2)

    println(s"Solution1: ${countOccupiedSeats(finalLayout1)}")
    println(s"Solution2: ${countOccupiedSeats(finalLayout2)}")
  }

  def finalLayout(
    initialLayout: SeatLayout,
    computeNextLayout: (SeatLayout) => SeatLayout
  ): SeatLayout = LazyList
    .iterate(initialLayout)(computeNextLayout)
    .sliding(2)
    .dropWhile(_.toList match {
      case head :: next :: Nil => !equalTo(head, next)
    })
    .next()
    .head

  def countOccupiedSeats(layout: SeatLayout): Int = layout.map(_.count(_ == '#')).sum

  def countOccupiedNeighbors(layout: SeatLayout, row: Int, column: Int): Int =
    (for {
      i <- Math.max(0, row - 1) until Math.min(row + 2, layout.length)
      j <- Math.max(0, column - 1) until Math.min(column + 2, layout(i).length)
      if !(i == row && j == column)
    } yield if (layout(i)(j) == '#') 1 else 0).sum

  val directions: Seq[(Int, Int)] = for {
    i <- -1 to 1
    j <- -1 to 1
    if !(i == 0 && j == 0)
  } yield (i, j)

  def addVector(position: (Int, Int), direction: (Int, Int)): (Int, Int) =
    (position._1 + direction._1, position._2 + direction._2)

  def countOccupiedVisibleSeats(layout: SeatLayout, row: Int, column: Int): Int = directions.count {
    direction =>
      LazyList
        .iterate((row, column))(addVector(_, direction))
        .drop(1)
        .takeWhile { case (i, j) =>
          i >= 0 && j >= 0 && i < layout.length && j < layout(0).length
        }
        .dropWhile { case (i, j) => layout(i)(j) == '.' }
        .headOption
        .exists { case (i, j) => layout(i)(j) == '#' }
  }

  def nextLayout(layout: SeatLayout): SeatLayout = layout.indices.map { i =>
    val row = layout(i)
    row.indices.map { j =>
      row(j) match {
        case '.'                                              => '.'
        case 'L' if countOccupiedNeighbors(layout, i, j) == 0 => '#'
        case '#' if countOccupiedNeighbors(layout, i, j) >= 4 => 'L'
        case c                                                => c
      }
    }.toArray
  }.toArray

  def nextLayout2(layout: SeatLayout): SeatLayout = layout.indices.map { i =>
    val row = layout(i)
    row.indices.map { j =>
      row(j) match {
        case '.'                                                 => '.'
        case 'L' if countOccupiedVisibleSeats(layout, i, j) == 0 => '#'
        case '#' if countOccupiedVisibleSeats(layout, i, j) >= 5 => 'L'
        case c                                                   => c
      }
    }.toArray
  }.toArray

  def equalTo(thisLayout: SeatLayout, thatLayout: SeatLayout): Boolean =
    thisLayout.zip(thatLayout).forall { case (a, b) => a.sameElements(b) }
}
