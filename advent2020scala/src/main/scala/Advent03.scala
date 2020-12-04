object Advent03 {
  def countTrees(inputs: List[String], stepRight: Int, stepDown: Int): Long = {
    var left: Int = 0
    var count: Long = 0

    for {
      i <- stepDown until inputs.length by stepDown
    } {
      val line = inputs(i)
      left += stepRight
      left %= line.length

      if (line.charAt(left) == '#') count = count + 1
    }

    count
  }

  def main(args: Array[String]): Unit = {
    val inputs = Input.byExercise(3)

    val count11 = countTrees(inputs, 1, 1)
    val count31 = countTrees(inputs, 3, 1)
    val count51 = countTrees(inputs, 5, 1)
    val count71 = countTrees(inputs, 7, 1)
    val count12 = countTrees(inputs, 1, 2)

    println(s"Solution1: $count31")
    println(s"Solution2: ${count11 * count31 * count51 * count71 * count12}")
  }
}
