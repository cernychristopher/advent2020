import scala.collection.immutable.BitSet

object Advent06 {
  type Group = List[String]

  def nextGroup(inputs: List[String]): (List[String], Group) = {
    val (groupLines, rest) = inputs.span(_.nonEmpty)

    (rest.dropWhile(_.isEmpty), groupLines)
  }

  def getGroups(inputs: List[String]): List[Group] = {
    val (rest, group) = nextGroup(inputs)

    rest match {
      case Nil => List(group)
      case _   => group :: getGroups(rest)
    }
  }

  def personBits(line: String) = {
    var bits = 0
    line.foreach { char =>
      bits |= (1 << (char - 'a'))
    }

    bits
  }

  def anyone(group: Group): Int = {
    var bits = 0
    group.foreach { line =>
      bits |= personBits(line)
    }

    (0 to 25).count { index =>
      ((1 << index) & bits) != 0
    }
  }

  def everyone(group: Group): Int = {
    var bits = 0x3ffffff
    group.foreach { line =>
      bits &= personBits(line)
    }

    (0 to 25).count { index =>
      ((1 << index) & bits) != 0
    }
  }

  def main(args: Array[String]): Unit = {
    val inputs = Input.byExercise(6)
    val groups = getGroups(inputs)

    println(s"Solution1: ${groups.map(anyone).sum}")
    println(s"Solution2: ${groups.map(everyone).sum}")
  }
}
