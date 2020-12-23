import Input.ListOps

object Advent23 {
  def main(args: Array[String]): Unit = {
    // val cups = Array(3, 2, 4, 1, 5)
    // val cups = Vector(3, 8, 9, 1, 2, 5, 4, 6, 7)
    val cups = Vector(2, 1, 9, 3, 4, 7, 8, 6, 5)

    val finalState = crabGame(cups)
      //.tapEach(println)
      .drop(100)
      .next()

    val solution1 = concatSolution(finalState._2)
    println(s"Solution1: ${solution1}")

    val extendedCups = cups ++ (cups.max + 1 to 1000000)
    val solution2State = crabGame(extendedCups).drop(10000000).next()._2
    val _ :: a :: b :: _ = reorder(solution2State, 1).toList

    val solution2 = a.toLong * b.toLong
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

  def reorder(cups: Vector[Int], currentCup: Int): Vector[Int] = {

    val (a, b) = cups.splitAt(cups.indexOf(currentCup))
    b ++ a
    // cups.dropWhile(_ != currentCup) ++ cups.takeWhile(_ != currentCup)
  }

  def concatSolution(cups: Vector[Int]): String = reorder(cups, 1).drop(1).mkString("")
}
