object Advent0 {
  def main(args: Array[String]): Unit = {
    val sumOfAllInputs = Input.byExercise(0).map(_.toInt).sum
    println(sumOfAllInputs)
  }
}
