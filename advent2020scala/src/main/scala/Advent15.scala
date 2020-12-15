import scala.collection.mutable

object Advent15 {
  def main(args: Array[String]): Unit = {
    val input = List(6, 19, 0, 5, 7, 13, 1)

    val solution1 = findNumberQuadratic(input, 2020) // 468
    println(s"Solution1: $solution1")

    val solution2 = findNumberLinear(input, 30000000) // 1801753
    println(s"Solution2: $solution2")
  }

  def findNumberQuadratic(input: List[Int], end: Int): Int =
    (input.size until end)
      .foldLeft(input.toVector) { (numbers, index) =>
        val lastNumber = numbers.last
        val lastNumberIndex = index - 1
        val spokenBefore = numbers.lastIndexOf(lastNumber, numbers.length - 2)

        numbers.appended(
          if (spokenBefore == -1) 0
          else lastNumberIndex - spokenBefore
        )
      }
      .last

  def findNumberLinear(input: List[Int], end: Int): Int = {
    val spokenBefore: mutable.Map[Int, Int] = mutable.Map.empty
    val lastNumbers: mutable.Map[Int, Int] = mutable.Map.from(input.zipWithIndex)

    (input.size until end).foldLeft(input.last) { (lastNumber, index) =>
      val nextNumber = spokenBefore.get(lastNumber) match {
        case Some(spokenBeforeIndex) => lastNumbers(lastNumber) - spokenBeforeIndex
        case None                    => 0
      }

      lastNumbers.put(nextNumber, index).foreach(spokenBefore.put(nextNumber, _))

      nextNumber
    }
  }
}
