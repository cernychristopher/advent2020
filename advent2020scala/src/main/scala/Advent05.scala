object Advent05 {
  def main(args: Array[String]): Unit = {

    val seatIds = Input
      .byExercise(5)
      .map { line =>
        val binary = line.replace('F', '0').replace('B', '1').replace('L', '0').replace('R', '1')
        Integer.parseInt(binary, 2)
      }
      .sorted

    val mySeatId = seatIds
      .sliding(2)
      .find { case lower :: higher :: Nil =>
        lower + 2 == higher
      }
      .map(_.head + 1)

    println(s"Solution1: ${seatIds.max}")
    println(s"Solution2: $mySeatId")
  }
}
