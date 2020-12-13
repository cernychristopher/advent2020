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
      .drop(1) // the first line is irrelevant to solution2
      .head
      .split(',')
      .map {
        case "x" => None
        case s   => Option(s.toInt)
      }
      .zipWithIndex // index: this bus should arrive index minutes _after the first bus_
      .collect { case (Some(bus), offset) =>
        (
          bus,
          (bus - offset % bus) % bus // Bus 17 arrives 2 minutes after the first bus === timestamp is in residue class (17 -2) mod 17
        )
      }

    println(allBusses.toList)

    val solution2 = allBusses
      .sortBy(-_._1) // looking at busses that arrive rarely first prunes the search space faster
      .foldLeft((0L, 1L)) { case ((start, step), (bus, offset)) =>
        // assumption: all the busses have no common divisors

        // start: the first number that follows the rules for all busses seen so far
        //   numbers that _also_ follow those rules are 'step' apart
        //
        // find the first number that follows the rules so far AND ALSO for this bus
        val nextStart = Iterator.iterate(start) { _ + step }.find { _ % bus == offset }.get

        (nextStart, step * bus)
      }
      ._1

    println(s"Solution2: $solution2")
  }
}
