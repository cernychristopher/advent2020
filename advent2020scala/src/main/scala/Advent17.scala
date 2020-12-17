object Advent17 {
  case class Point(x: Int, y: Int, z: Int, w: Int)

  def main(args: Array[String]): Unit = {
    val initialActivePoints = Input
      .byExercise(17)
      .reverse
      .zipWithIndex
      .flatMap { case (line, y) =>
        line.toCharArray.zipWithIndex.flatMap { case (char, x) =>
          char match {
            case '#' => Some(Point(x, y, z = 0, w = 0))
            case _   => None
          }
        }
      }
      .toSet

    val finalState3d = cycles(initialActivePoints, allClosePoints3d).drop(6).next()
    val finalState4d = cycles(initialActivePoints, allClosePoints4d).drop(6).next()

    println(s"Solution2: ${finalState3d.size}")
    println(s"Solution2: ${finalState4d.size}")
  }

  def cycles[P](init: Set[P], neighbors: P => Set[P]): Iterator[Set[P]] =
    Iterator.iterate(init) { activePoints =>
      activePoints
        .flatMap { point => neighbors(point) + point }
        .filter { point =>
          val activeNeighbors = neighbors(point).count { closePoint =>
            activePoints.contains(closePoint)
          }

          if (activePoints.contains(point)) activeNeighbors == 2 || activeNeighbors == 3
          else activeNeighbors == 3
        }
    }

  def allClosePoints3d(point: Point): Set[Point] = (
    for {
      x <- point.x - 1 to point.x + 1
      y <- point.y - 1 to point.y + 1
      z <- point.z - 1 to point.z + 1
      if !(x == point.x && y == point.y && z == point.z)
    } yield Point(x, y, z, 0)
  ).toSet

  def allClosePoints4d(point: Point): Set[Point] = (
    for {
      x <- point.x - 1 to point.x + 1
      y <- point.y - 1 to point.y + 1
      z <- point.z - 1 to point.z + 1
      w <- point.w - 1 to point.w + 1
      if !(x == point.x && y == point.y && z == point.z && w == point.w)
    } yield Point(x, y, z, w)
  ).toSet
}
