import Input.ListOps

object Advent06 {
  type Group = List[String]

  def personBits(line: String): Int = {
    var bits = 0
    line.foreach { char =>
      bits |= (1 << (char - 'a'))
    }

    bits
  }

  def countBits(bits: Int): Int = {
    (0 to 25).count { index =>
      ((1 << index) & bits) != 0
    }
  }

  def anyone(group: Group): Int = {
    var bits = 0

    group.foreach { line =>
      bits |= personBits(line)
    }

    countBits(bits)
  }

  def everyone(group: Group): Int = {
    var bits = 0x3ffffff

    group.foreach { line =>
      bits &= personBits(line)
    }

    countBits(bits)
  }

  def main(args: Array[String]): Unit = {
    val groups = Input.byExercise(6).separatedBy(_.isEmpty)

    println(s"Solution1: ${groups.map(anyone).sum}")
    println(s"Solution2: ${groups.map(everyone).sum}")
  }
}
