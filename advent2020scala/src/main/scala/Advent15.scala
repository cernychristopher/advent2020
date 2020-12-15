object Advent15 {
  def main(args: Array[String]): Unit = {
    val input = List(6, 19, 0, 5, 7, 13, 1)

    val spokenNumbers = (input.size until 2020).foldLeft(input.toVector) { (numbers, index) =>
      val lastNumber = numbers.last
      val lastNumberIndex = index - 1
      val spokenBefore = numbers.lastIndexOf(lastNumber, numbers.length - 2)

      numbers.appended(
        if (spokenBefore == -1) 0
        else lastNumberIndex - spokenBefore
      )
    }

    val solution1 = spokenNumbers.last
    println(s"Solution1: $solution1")

    val spokenNumbers2 =
      (input.size until 30000000).foldLeft(
        input.zipWithIndex.map(pair => pair._1 -> List(pair._2)).toMap -> input.last
      ) { case ((numbers, lastNumber), index) =>
        val indices = numbers.getOrElse(lastNumber, Nil)
        val nextNumber = indices match {
          case _ :: Nil                       => 0
          case lastIndex :: spokenBefore :: _ => lastIndex - spokenBefore
        }

        numbers.updated(nextNumber, index :: numbers.getOrElse(nextNumber, Nil)) -> nextNumber
      }

    val solution2 = spokenNumbers2._2
    println(s"Solution2: $solution2")
  }
}
