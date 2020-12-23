object Advent23 {
  def main(args: Array[String]): Unit = {
    val cups = List(2, 1, 9, 3, 4, 7, 8, 6, 5)

    val finalState = crabGameFast(cups)
      .drop(100)
      .next()

    val solution1 = next(finalState._2, 1, 8).mkString("")
    println(s"Solution1: $solution1")

    val extendedCups = cups ++ (cups.max + 1 to 1000000)
    val solution2State = crabGameFast(extendedCups).drop(10000000).next()._2
    val afterOne = next(solution2State, 1, 2)

    val solution2 = afterOne.map(_.toLong).product
    println(s"Solution2: $solution2")
  }

  private def next(cups: Map[Int, Int], from: Int, amount: Int): List[Int] =
    if (amount <= 0) Nil
    else {
      val nextNumber = cups(from)
      nextNumber :: next(cups, nextNumber, amount - 1)
    }

  def crabGameFast(cupList: List[Int]): Iterator[(Int, Map[Int, Int])] = {
    val circularCupList =
      cupList
        .sliding(2, 1)
        .map { window => window.head -> window(1) }
        .toMap + (cupList.last -> cupList.head)

    val cupSize = cupList.size

    Iterator
      .iterate(cupList.head -> circularCupList) { case (currentCup, cups) =>
        val selectedCups = next(cups, currentCup, 3)

        val destinationCup =
          (2 to 5)
            .map { sub => (currentCup - sub + cupSize) % cupSize + 1 }
            .find(!selectedCups.contains(_))
            .get

        val result =
          cups +
            (currentCup -> cups(selectedCups(2))) +
            (destinationCup -> selectedCups.head) +
            (selectedCups(2) -> cups(destinationCup))

        result(currentCup) -> result
      }
  }
}
