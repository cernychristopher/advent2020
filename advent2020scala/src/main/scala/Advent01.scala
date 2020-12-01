object Advent01 {
  def main(args: Array[String]): Unit = {
    val inputs = Input.byExercise(1).map(_.toInt)

    // find the two numbers in the expense report that sum to 2020, and multiply them
    inputs.combinations(2)
      .find(_.sum == 2020)
      .map(_.product)
      .foreach(println)

    // ... and the same with three numbers
    inputs.combinations(3)
      .find(_.sum == 2020)
      .map(_.product)
      .foreach(println)
  }
}
