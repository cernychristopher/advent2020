object Advent13 {
  def main(args: Array[String]): Unit = {
    val input = Input.byExercise(13)

    val (earliestDeparture, busses) = input match {
      case head :: second :: Nil =>
        (head.toInt, second.split(',').filterNot(_ == "x").map(_.toInt).toList)
    }

    val actualDeparture = LazyList.from(earliestDeparture).find { departure =>
      busses.exists(bus => departure % bus == 0)
    }

    val waitTime = actualDeparture.map(_ - earliestDeparture)

    val bus = actualDeparture.flatMap { departure => busses.find(bus => departure % bus == 0) }

    val solution1 = for {
      wt <- waitTime
      b <- bus
    } yield wt * b

    println(s"Solution1: $solution1")

    val allBusses = input
      .drop(1)
      .head
      .split(',')
      .map {
        case "x" => None
        case s   => Option(s.toInt)
      }
      .zipWithIndex
      .collect { case (Some(bus), offset) =>
        (bus, (10 * bus - offset) % bus)
      }
      .sortBy(_._1)
      .reverse

    val solution2 = allBusses
      .foldLeft((0L, 1L)) { case ((start, product), (bus, offset)) =>
        val nextStart = Iterator.iterate(start) { _ + product }.find { _ % bus == offset }.get
        (nextStart, product * bus)
      }
      ._1

    println(s"Solution2: $solution2")
  }
}
