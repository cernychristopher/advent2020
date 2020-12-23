object Advent23 {
  def main(args: Array[String]): Unit = {
    val cups = Vector(2, 1, 9, 3, 4, 7, 8, 6, 5)

    val finalState = crabGame(cups)
      .drop(100)
      .next()

    val solution1 = concatSolution(finalState._2)
    println(s"Solution1: ${solution1}")

    val extendedCups = cups ++ (cups.max + 1 to 1000000)
    val solution2State = crabGameFast(extendedCups).drop(10000000).next()._2
    val afterOne = next(solution2State, 1, 2)

    println(afterOne)

    val solution2 = afterOne.map(_.toLong).product
    println(s"Solution2: ${solution2}")
  }

  def crabGame(cups: Vector[Int]): Iterator[(Int, Vector[Int])] = {
    val cupModulus = cups.size
    var iteration = 0
    var time = System.currentTimeMillis()

    Iterator
      .iterate(cups.head -> cups) { case (currentCup, cups) =>
        if (iteration % 100 == 0) {
          val newTime = System.currentTimeMillis()
          println(s"$iteration: ${newTime - time}")
          time = newTime
        }
        iteration += 1

        val orderedCups = reorder(cups, currentCup)
        val selectedCups = orderedCups.slice(1, 4)
        val rest = orderedCups.take(1) ++ orderedCups.drop(4)
        val destinationCup =
          (2 to 5)
            .map { sub => (currentCup - sub + cupModulus) % cupModulus + 1 }
            .find(!selectedCups.contains(_))
            .get

        val (before, afterIncluding) = rest.splitAt(rest.indexOf(destinationCup))
        // val before :: after :: _ = rest.toList.separatedBy(_ == destinationCup)
        val result = before ++ Vector(destinationCup) ++ selectedCups ++ afterIncluding.drop(1)
        val newCurrentCup = result((result.indexOf(currentCup) + 1) % result.size)

        newCurrentCup -> result
      }
  }

  private def next(cups: Map[Int, Int], from: Int, amount: Int): List[Int] =
    if (amount <= 0) Nil
    else {
      val nextNumber = cups(from)
      nextNumber :: next(cups, nextNumber, amount - 1)
    }

  def crabGameFast(cups: Vector[Int]): Iterator[(Int, Map[Int, Int])] = {
    val cupOrder =
      cups.sliding(2, 1).map { window => window.head -> window(1) }.toMap + (cups.last -> cups.head)

    val cupModulus = cups.size

    Iterator
      .iterate(cups.head -> cupOrder) { case (currentCup, cups) =>
        val selectedCups = next(cups, currentCup, 3)

        val destinationCup =
          (2 to 5)
            .map { sub => (currentCup - sub + cupModulus) % cupModulus + 1 }
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

  def reorder(cups: Vector[Int], currentCup: Int): Vector[Int] = {
    val (a, b) = cups.splitAt(cups.indexOf(currentCup))
    b ++ a
  }

  def concatSolution(cups: Vector[Int]): String = reorder(cups, 1).drop(1).mkString("")
}
