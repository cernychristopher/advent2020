object Advent24 {
  sealed trait Direction
  case object SouthEast extends Direction
  case object NorthEast extends Direction
  case object SouthWest extends Direction
  case object NorthWest extends Direction
  case object East extends Direction
  case object West extends Direction

  case class CenterPosition(top: Int, left: Int)

  def main(args: Array[String]): Unit = {
    val directions = Input.byExercise(24).map(_.toList).map(toDirections)

    val centerPositions = directions.map(toPosition(CenterPosition(0, 0)))
    val blackTiles = centerPositions
      .groupBy(identity)
      .view
      .mapValues(_.length)
      .filter(_._2 % 2 == 1)
      .keySet

    val solution1 = blackTiles.size
    println(s"Solution1: $solution1")

    val solution2 = Iterator
      .iterate(blackTiles) { tiles =>
        tiles
          .flatMap(withNeighbors)
          .flatMap { candidateTile =>
            val isBlack = tiles.contains(candidateTile)
            val blackNeighbors = neighbors(candidateTile).count(tiles.contains)

            if (isBlack) {
              if (blackNeighbors == 0) None
              else if (blackNeighbors > 2) None
              else Option(candidateTile)
            } else if (blackNeighbors == 2) {
              Option(candidateTile)
            } else
              None
          }
      }
      .map(_.size)
      .drop(100)
      .next()

    println(s"Solution2: $solution2")
  }

  def toDirections(in: List[Char]): List[Direction] = in match {
    case Nil                => Nil
    case 'e' :: tail        => East :: toDirections(tail)
    case 'w' :: tail        => West :: toDirections(tail)
    case 's' :: 'e' :: tail => SouthEast :: toDirections(tail)
    case 'n' :: 'e' :: tail => NorthEast :: toDirections(tail)
    case 's' :: 'w' :: tail => SouthWest :: toDirections(tail)
    case 'n' :: 'w' :: tail => NorthWest :: toDirections(tail)
  }

  def toPosition(start: CenterPosition)(directions: List[Direction]): CenterPosition =
    Function.tupled(CenterPosition.apply _)(directions.foldLeft(start.top -> start.left) {
      case ((top, left), direction) =>
        direction match {
          case East      => top -> (left + 2)
          case West      => top -> (left - 2)
          case NorthEast => (top - 3) -> (left + 1)
          case SouthWest => (top + 3) -> (left - 1)
          case SouthEast => (top + 3) -> (left + 1)
          case NorthWest => (top - 3) -> (left - 1)
        }
    })

  private val allDirections =
    List(East, West, NorthWest, NorthEast, SouthWest, SouthEast).map(_ :: Nil)

  def neighbors(position: CenterPosition): List[CenterPosition] =
    allDirections.map(toPosition(position))

  def withNeighbors(position: CenterPosition): List[CenterPosition] =
    position :: neighbors(position)
}
