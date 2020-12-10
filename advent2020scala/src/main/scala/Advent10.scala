import Input.ListOps

object Advent10 {
  def main(args: Array[String]): Unit = {
    val input = Input.byExercise(10).map(_.toLong)
    val sorted = (input.max + 3 :: 0L :: input).sorted

    val oneDiffs = sorted.sliding(2).count { case from :: to :: Nil =>
      to - from == 1
    }

    val threeDiffs = sorted.sliding(2).count { case from :: to :: Nil =>
      to - from == 3
    }

    val solution1 = oneDiffs * threeDiffs

    val solution2 = sorted
      .sliding(2)
      .toList
      .separatedBy { case from :: to :: Nil =>
        to - from == 3
      } // If two adapters are three jolts apart, we cannot leave them out!
      .filterNot(
        _.forall(_.isEmpty)
      ) // those empty groups are produced by sequences of jolts that are three apart
      .map(_.flatten.distinct.sorted) // (4,5), (5,6), (6,7) -> (4,5,6,7)
      .map { group =>
        val start = group.head
        val end = group.last
        // the first and last element in a group cannot be left out - they form
        // the bridge to the next group, which is 3 jolts apart
        val innerAdapters = group.drop(1).dropRight(1)

        // test which of the _inner_ adapters can be left out
        powerset(innerAdapters).count { subset =>
          isValidAdapterSequence(start :: subset ::: List(end))
        }
      }
      .map(BigInt.apply) // the solution is HUGE this time
      .product

    println(s"Solution1: $solution1")
    println(s"Solution2: $solution2")
  }

  def isValidAdapterSequence(list: List[Long]): Boolean =
    list.sliding(2).forall { case from :: to :: Nil =>
      to - from <= 3
    }

  def powerset[A](s: List[A]): Iterator[List[A]] =
    (0 to s.size).map(s.combinations).reduce(_ ++ _)
}
