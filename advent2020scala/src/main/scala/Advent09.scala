object Advent09 {
  def main(args: Array[String]): Unit = {
    val input = Input.byExercise(9).map(_.toLong)
    val solution1 = input
      .sliding(26, 1)
      .find { window =>
        val lastNumber = window.last

        !window.dropRight(1).combinations(2).exists(_.sum == lastNumber)
      }
      .flatMap(_.lastOption)
      .get

    val contiguousList = Iterator
      .from(2)
      .flatMap {
        input.sliding(_, 1).find(_.sum == solution1)
      }
      .next()

    val solution2 = contiguousList.min + contiguousList.max

    println(s"Solution1: $solution1")
    println(s"Solution2: $solution2")
  }
}
